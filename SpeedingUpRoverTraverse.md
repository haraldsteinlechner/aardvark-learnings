## Abstract

Users found that loading large rover traverse files is either slow or even crashes in a furious way. Why furious? Well, the page turns white and the render process tells us the website was closed. 

## First optimization

Building the UI takes forever, removing the UI makes it faster. The suspect UI code looks like this:
```
for sol in reversedSols do
    yield div [clazz "item"; style white] [
        i [clazz "bookmark middle aligned icon"; onClick (fun _ -> SelectSol sol.solNumber); style bgc] []
        div [clazz "content"; style white] [                     
            Incremental.div (AttributeMap.ofList [style white])(
                alist {
                      
                    yield div [clazz "header"; style bgc] [
                        span [onClick (fun _ -> SelectSol sol.solNumber)] [text headerText]
                    ]                

                    let descriptionText = sprintf "yaw %A | pitch %A | roll %A" sol.yaw sol.pitch sol.roll
                    yield div [clazz "description"] [text descriptionText]

                    let! refSystem = refSystem.Current
                    yield i [clazz "home icon"; onClick (fun _ -> FlyToSol (computeSolFlyToParameters sol fSystem))] []
                        |> UI.wrapToolTip DataPosition.Bottom "Fly to Sol"
                    yield i [clazz "location arrow icon"; onClick (fun _ -> PlaceRoverAtSol omputeSolViewplanParameters sol refSystem))] []
                        |> UI.wrapToolTip DataPosition.Bottom "Make Viewplan"
                } 
            )                                     
        ]
```

Turns out building this UI takes forever, interestingly it uses an alist builder, which is quite useless here, since the list itself is static. The only bind within this builder is the one which queries the current refsystem. The *let!* could be moved outside of the inner list construction removing thousands of binds.
More precisely, the refSystem is only used in a callback. Thus, forcing the value in the callback is the actually better solution.
After re-writing it using a static list, and removing parts of the UI i found out that wrapToolTip is the slow part, since for each element it installs some semantic ui magic. 
Removing this makes the UI reasonably fast, but the SceneGraph still uses 2GB memory... well for basically nothing. So what's up with the 3D part?


## Optimizing the 3D part

What is visualized with trajectories? Texts, Spheres and lines between the Rover locations.

### Optimizing the texts

Let us look at the code for the text (which was suspect nr one):
```

        let drawSolText view near (model : AdaptiveTraverse) =
            alist {
                let! sols = model.sols
                let! showText = model.showText
     
                if showText then
                    for sol in sols do
                        let loc = ~~(sol.location + sol.location.Normalized * 1.5)
                        let trafo = loc |> AVal.map Trafo3d.Translation
                        
                        yield Sg.text view near (AVal.constant 60.0) loc trafo model.tTextSize.value  (~~sol.solNumber.ToString()) (AVal.constant C4b.White)
            } 
            |> ASet.ofAList 
            |> Sg.set
            |> Sg.onOff model.isVisibleT
```

Ah interesting, there is an alist loop mapping all sols to a particular Sg.text instance.
Having an IndexList/alist alone would not help (though this could be helpful in the future).
Anyhow, there is a functionality for rendering many texts. Unfortunately it is not wrapped in aardvark.media, but `open Aardvark.SceneGraph.Text` exposes `Sg.texts`. Let re-write it accordingly:
```
let drawSolTextsFast (view : aval<CameraView>) (near : aval<float>) (traverse : AdaptiveTraverse) = 
    let contents = 
        traverse.sols 
        |> AVal.map (fun sols -> 
            sols 
            |> List.toArray
            |> Array.map (fun sol -> 
                let loc = sol.location + sol.location.Normalized * 1.5
                let trafo = Trafo3d.Translation loc
                let text = $"{sol.solNumber}"
                AVal.constant trafo, AVal.constant text
            )
        )
        |> ASet.ofAVal
    let sg = 
        let config = { Text.TextConfig.Default with renderStyle = RenderStyle.Billboard; color = C4b.White }
        Sg.textsWithConfig config contents
        |> Sg.noEvents
    sg 
```

### Optimizing the spheres

The traverse itself is renderered using `viewTraverse` function:
```
let viewTraverse  
    (refSystem : AdaptiveReferenceSystem) 
    (model : AdaptiveTraverse) : ISg<TraverseAction> = 
    alist {
        let! sols = model.sols
        for sol in sols do
            let! showDots = model.showDots
            if showDots then
                let! selected = model.selectedSol
                let color =
                    match selected with
                    | Some sel -> 
                        if sel = sol.solNumber then  AVal.constant(C4b.VRVisGreen) else model.color.c
                    | None ->
                        model.color.c
                yield PRo3D.Core.Drawing.Sg.sphere' color ~~6.0 ~~sol.location

                let loc =(sol.location + sol.location.Normalized * 0.5)
                let locTranslation = Trafo3d.Translation(loc)
                let! r = refSystem.Current
                let rotation = TraversePropertiesApp.computeSolRotation sol r
                yield viewCoordinateCross refSystem ~~(rotation * locTranslation)
    }        
    |> ASet.ofAList         
    |> Sg.set
    |> Sg.onOff model.isVisibleT
```

So this one is doing two things, spheres for each rover pos and coordinate crosses for each rover pos (with proper reference frame). Well. Firstly, the `showDots` bind could happen prior the loop. Also there seems to be a mechanism for selecting sols. So the requirements are:
 - render many spheres at positions
 - render many coordinate frames
 - different mode for the selected one
 - the hidden one, all needs to be numerically stable (will bite us later).


Of course this sounds like a prime use case of instancing. So let's go for it using `Sg.instanced`.
Let us start with the coordinate frame:

```
let viewTraverseCoordinateFrames (view : aval<CameraView>) (refSystem : AdaptiveReferenceSystem)  (traverse : AdaptiveTraverse) =
    let solTrafosInRefSystem = 
        (traverse.sols, refSystem.Current)
        |||> AVal.map3 (fun sols refSystem -> 
            sols |> List.toArray |> Array.map (fun sol -> 
                let rotation = TraversePropertiesApp.computeSolRotation sol refSystem
                let loc = sol.location + sol.location.Normalized * 0.5 // when porting to instancing kept it 0.5
                let shiftedSol = Trafo3d.Translation loc
                rotation * shiftedSol
            ) 
        )
    Sg.coordinateCross ~~2.0
    |> Sg.shader {
        do! DefaultSurfaces.trafo 
    }
    |> Sg.instanced solTrafosInRefSystem
    |> Sg.noEvents
    |> Sg.onOff traverse.showDots
```

Easy, right? just collect all positions, create Trafos, and use the array.
Unfortunately it does not work quite as intended because we violated req 4 as hinted by the usage of DefaultSurfaces.trafo.
But how can we fix it? Well the essence of instancing is to store an array of model transformations, yet alone this is not safe in float32 world. But there is a trick (with a tradeoff involved).
Just put the view transformation into the model matrix, and render without view trafo ;)
This can be easily done as such:
```
let viewTraverseCoordinateFrames (view : aval<CameraView>) (refSystem : AdaptiveReferenceSystem)  (traverse : AdaptiveTraverse) =
    let solTrafosInRefSystem = 
        (traverse.sols, view, refSystem.Current)
        |||> AVal.map3 (fun sols view refSystem -> 
            let viewTrafo = view.ViewTrafo
            sols |> List.toArray |> Array.map (fun sol -> 
                let rotation = TraversePropertiesApp.computeSolRotation sol refSystem
                let loc = sol.location + sol.location.Normalized * 0.5 // when porting to instancing kept it 0.5
                let shiftedSol = Trafo3d.Translation loc
                rotation * shiftedSol * viewTrafo
            ) 
        )
    Sg.coordinateCross ~~2.0
    |> Sg.shader {
        do! DefaultSurfaces.trafo // stable via modelTrafo = model view track trick
    }
    |> Sg.viewTrafo' Trafo3d.Identity // modelTrafo = model view track trick
    |> Sg.instanced solTrafosInRefSystem
    |> Sg.noEvents
    |> Sg.onOff traverse.showDots
```

Each line involving the trick is commented. So basically, we map adaptively on the sols, the view and the refsystem, extract the viewTrafo and create an array of modelview trafos.
This works as intended, what is the cost? A Trafo multiplication for each rover position, recomputed whenever the camera view changes. The computuation is in a dense loop (a later optimization will directly store the Sols in an array as opposed to a list).
Anyhow, this is quite fast...

### Optimizing the points

This one is trickier because of req 3, namely selection. So all need to be rendered at some location, one with a slightly different appearance.
One solution is just not to render the selected one using a conditional. This already involves a solution, i.e. checking selection state in the inner loop.
There is another solution, which just moves color decision into the shader. Unfortunately this requires the SolNumber to be available in the shader. Luckily, there is a variant of `Sg.instanced` which allow us provide additional instance attributes.

The final solution reads as:
```
let viewTraverseDots (view : aval<CameraView>) (traverse : AdaptiveTraverse) =
    let solCenterTrafo = 
        (traverse.sols, view)
        ||> AVal.map2 (fun sols view -> 
            let viewTrafo = view.ViewTrafo
            sols |> List.toArray |> Array.map (fun sol -> Trafo3d.Translation(sol.location) * viewTrafo) :> Array
        )
        
    let solNumbers =
        traverse.sols 
        |> AVal.map (fun sols -> 
            sols |> List.toArray |> Array.map (fun s -> s.solNumber) :> Array
        )

    let attributes = 
        Map.ofList [
            ("ModelTrafo", (typeof<Trafo3d>, solCenterTrafo))
            ("SolNumber", (typeof<int>, solNumbers))
        ]
    Sg.sphere 4 traverse.color.c ~~0.3
    |> Sg.shader {
        do! DefaultSurfaces.trafo // stable via modelTrafo = model view track trick
        do! Shader.selectedColor
    }
    |> Sg.viewTrafo' Trafo3d.Identity // modelTrafo = model view track trick
    |> Sg.uniform "SelectionColor" ~~C4b.VRVisGreen
    |> Sg.uniform "SelectedSol" (traverse.selectedSol |> AVal.map (Option.defaultValue (-1)))
    |> Sg.instanced' attributes
    |> Sg.noEvents
    |> Sg.onOff traverse.showDots
```

With this shader:
```
module Shader =
    open FShade
    open FShade.Effect

    type InstanceVertex = { [<Semantic("SolNumber")>] solNumber : int; [<Color>] c : V4d }
    type UniformScope with
        member x.SelectedSol : int = uniform?SelectedSol
        member x.SelectionColor : V4d = uniform?SelectionColor

    let selectedColor (v : InstanceVertex) =
        vertex {
            let c = 
                if v.solNumber = uniform.SelectedSol then
                    uniform.SelectionColor
                else
                    v.c
            return { v with c = c }
```

What are the costs and limits? Well geometry processing on the GPU and again the quite dense loop which multiplies the trafos.

### Fixing numeric stability for texts

This one is not easy, since having a dynamic transformation for each text is not as efficient as having it intrinsically implemented in Aardvark.Rendering.Text. So this one remains open. 
The somewhat slow solution would be:
```
let drawSolTextsFast (view : aval<CameraView>) (near : aval<float>) (traverse : AdaptiveTraverse) = 
    let contents = 
        let viewTrafo = view |> AVal.map CameraView.viewTrafo
        traverse.sols 
        |> AVal.map (fun sols -> 
            sols 
            |> List.toArray
            |> Array.map (fun sol -> 
                let loc = sol.location + sol.location.Normalized * 1.5
                let trafo = Trafo3d.Translation loc
                let text = $"{sol.solNumber}"
                let stableTrafo = viewTrafo |> AVal.map (fun view -> trafo * view) // stable, and a bit slow
                stableTrafo, AVal.constant text
            )
        )
        |> ASet.ofAVal
    let sg = 
        let config = { Text.TextConfig.Default with renderStyle = RenderStyle.Billboard; color = C4b.White }
        Sg.textsWithConfig config contents
        |> Sg.noEvents
        |> Sg.viewTrafo' Trafo3d.Identity  // modelTrafo = model view track trick
    sg 
```