// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open SdlDotNet.Core
open SdlDotNet.Graphics  
open System.Drawing
open System.Threading
open SdlDotNet.Audio

type Element = Fire 
             | Ice 
             | Lightning 

type Weapons = 
   | EnchantedSword of Element 
   | Arrow of Element 
   | Sword 
   | Nothing

type Actions = Move of int 
             | PickUp 
             | AttackMode 
             | Freeze

type Item = Weapon of Weapons | Potion of float | Tree

type Hero = {Intelligence : int ; 
             Strength: int ; 
             MaxHealth : int; 
             WeakTo : Element;
             Dexterity : float; 
             mutable Health : int; 
             mutable Position : (int * int)
             mutable Equipped : Weapons}

let Elements = [|Fire ; Ice; Lightning|] 

let mapwidth , mapheight = 10, 5

let font = new SdlDotNet.Graphics.Font(@"C:\Windows\Fonts\impact.ttf",42);

let random = Random()

let white = Some Drawing.Color.White

//////////////////////////////////////////

let (|LessZero | AboveZero|) h = if h > 0 then AboveZero else LessZero

let dist (x1, y1) (x2, y2) = abs(x1 - x2) + abs(y1 - y2)

let rect a b c d = Drawing.Rectangle(a,b,c,d)

let intdiv (i:int) (j:int) = (float i / float j) |> round |> int

let isArrow = function | Arrow(_) -> true | _ -> false

let wrap y x = if x < 0 then y else x

let flip f x y = f y x 

let point w h (x,y) = Drawing.Point(x * w,y * h) 

//////////////////////////////
let checkMap (curMap:Map<int*int, Item>) (x,y) item = 
  if curMap.ContainsKey (x,y) && curMap.[x,y] = item  then true else false

let isBlocked curMap (x,y) (x2,y2) = 
   checkMap curMap (x,y) Tree || (x,y) = (x2,y2)  

let messArrow a = if a = 0. then 0. elif a > 1. then 1./ a else a

let damage p d apart wep = if random.NextDouble() < p * (if isArrow wep then messArrow (4./apart) else 1.) then random.Next(0, d/2) else 0

let move () = Move(random.Next(0,3)) 

let compareWeapons (h : Hero) (enemy:Hero) = function
  | (pwep, Weapon(w)) -> 
    match (pwep, w) with
      | (w1,w2) when w1 = w2 -> 0
      | (EnchantedSword(el), EnchantedSword(el2)) when el = enemy.WeakTo -> 0
      | (EnchantedSword(_), EnchantedSword(el)) when el = enemy.WeakTo -> 100
      | (Arrow(el), Arrow(el2)) when el2 = enemy.WeakTo -> if isArrow enemy.Equipped then 60 else 50
      | w , Arrow(_) when (float enemy.Health / float enemy.MaxHealth) < 0.3 || (isArrow enemy.Equipped && (float h.Health/ float h.MaxHealth) <= 0.5) -> if isArrow w then 0 else 80
      | Arrow(_) , Arrow(_) 
      | Arrow(_) , Sword
      | Sword, Sword
      | EnchantedSword(_), _
      | EnchantedSword(_), EnchantedSword(_) -> 0
      | (_, EnchantedSword(_)) -> 70
      | _, Arrow(_) -> 30
      | _ , Sword -> 20
      | _ -> 0
  | _,Potion p -> 0 + ((p * (float h.MaxHealth / float h.Health)) ** (float h.Intelligence * 0.1 /  3.) |> round |> int)
  | _ , Tree -> 0

let pseudoMove (x,y) = function | Move 0 -> (x-1) |> wrap mapwidth, y 
                                | Move 1 -> (x+1)%mapwidth,y 
                                | Move 2 -> x, (y-1) |> wrap mapheight 
                                | Move _ -> x , (y + 1) % mapheight 
                                | _ -> x , y 

//////////////////////////////////////////////////

let think col (surf:Surface) (curMap:Map<int * int, Item>) oldpath (oldScore:int) (self:Hero) (enemy:Hero) = 
    let maxdepth = float(self.Intelligence) * 0.1 |> round |> int 
  
    let considerOption (p1:Hero) p2 = 
        let rec seek spos (vset:Set<int * int>) move =
          let x,y = spos 
//          surf.Fill( rect (x * 31) (y * 36) 30 30, col )
//          surf.Update()
//          Thread.Sleep(10)
          let wscore, hasKey = if curMap.ContainsKey(spos) then 
                                    //surf.Fill( rect (x * 31) (y * 36) 20 20, Drawing.Color.Yellow); 
                                    (compareWeapons p1 p2 (p1.Equipped,curMap.[spos])), true 
                               else 0, false
          function
            | depth when depth >= maxdepth || wscore > 0 || (hasKey && curMap.[spos] = Tree) -> if wscore > 0 then [PickUp, wscore] else []
            | depth ->  let moveset = [for i in 0..3 -> Move i, pseudoMove spos (Move i)] |> List.filter ((flip (snd >> Set.contains) vset ) >> not)
                        if moveset.Length > 0 then
                            let vset' = moveset |> List.map snd |> List.fold (flip Set.add) vset
                            let options = moveset |> List.map (fun (act,cpos) -> let slist = seek cpos vset' act (depth + 1)
                                                                                 let _ , score = if slist = [] then Freeze, 0 else slist.Head
                                                                                 [act, score] @ slist)
                            let topPath = List.maxBy (List.map snd) options
                            topPath
                        else []
        seek p1.Position (set [p1.Position]) Freeze 0 
  
    let hitp, planp = if (self.Equipped = Nothing && curMap <> Map.empty) || (float self.Health/float self.MaxHealth) < 0.3 then
                           0.2, 0.8
                      else 0.8, 0.2  //(match p1.Equipped with | Arrow(_) -> 3 | _ -> 1) * ((p1.Dexterity * 0.01 * ((5. + (float p1.Strength))/2.)) |> int)  
    let p = random.NextDouble()
    if p < hitp then
        [AttackMode],  (match self.Equipped with | Arrow(_) -> 3 | EnchantedSword(_) -> 2 |  _ -> 1) * ((self.Dexterity * 0.01 * ((5. + (float self.Strength))/2.)) |> int)  
    else 
        let paths = considerOption self enemy 
        let score = if paths = [] then 0 else (snd paths.Head)
        if score <= 0 || paths = [] then
           [AttackMode] , 0
        else 
            paths |> List.fold (fun (sx,sy) (act,_) ->
                                                   let (x,y) = pseudoMove (sx,sy) act 
                                                   surf.Fill( rect (sx * 31) (sy * 36) 24 24, Drawing.Color.BlueViolet )
                                                   surf.Update()
                                                   x,y) self.Position |> ignore
            System.Threading.Thread.Sleep(300)
            if score > oldScore || oldpath = [] then paths |> List.map fst , score else oldpath, oldScore

//////////////////////////////////////////////////////////////////////////////////////////// 

let sprite (str:string) doColor = 
     let spr = new Sprites.Sprite(str)
     match doColor with 
         | Some color -> spr.TransparentColor <- color
                         spr.Transparent <- true
         | None -> ()
     spr

let splat = sprite "splat.bmp" white
let grass = sprite "grass.bmp" None
let w, h = grass.Width , grass.Height 

let arrow = sprite "arrow.png" None
 
let pairmap f (x,y) = f x, f y

let taut _ = true
             
let line (x1,y1) (x2,y2) lastSurf f cond =  
    let xdir = sign (x2 - x1)  
    let rec loop x y csurf = 
       if x = x2 then true
       else
           let x' = x + xdir
           let y' = y1 + (y2 - y1) * (x' - x1)/(x2 - x1) 
           if cond (x', y') then false 
           else let nsurf = f (x, y, x', y', csurf)
                loop x' y' nsurf
    loop x1 y1 lastSurf

let lineofSight curmap (x1,y1) (x2,y2) = 
    line (x1,y1) (x2,y2) () (fun _ -> ()) (fun pos -> checkMap curmap pos Tree)

//let shootArrow (x1,y1) (x2,y2) = 
Mixer.ChannelsAllocated <- 100

let surface = Video.SetVideoMode(grass.Width * (mapwidth + 1),grass.Height * (mapheight + 1)) 
Video.WindowCaption <- "AI Fight!" 

let radToDeg r =   ((r * 180.) / Math.PI) |> round |> int

let drawArrow (x0,y0) (lastSurface:Surface option) (x,y) (arrowSurf:Surface)  = 
   let b = new Surface(w,h) 
   if lastSurface.IsSome then surface.Blit(lastSurface.Value,point w h (x0,y0))  |> ignore
   b.Blit(surface,Point(0,0), rect (x * w) (y*h) w h) |> ignore
   surface.Blit(arrowSurf,point w h (x,y))  
   b

let arrowAnim (x0,y0) (x1,y1) =
  let angle = atan2 (float((mapheight - y1) - (mapheight - y0) )) (float((x1 - x0)))
  let rotsurf = arrow.Surface.CreateRotatedSurface(radToDeg angle) 
  let surfnew = drawArrow (x0,y0) None (x0,y0) rotsurf
  line (x0,y0) (x1,y1) surfnew (fun (x0, y0, x, y, oldsurf) -> 
                                       let prevsurf = drawArrow (x0, y0) (Some(oldsurf)) (x,y) rotsurf
                                       surface.Update(); Thread.Sleep(100) 
                                       prevsurf) (taut >> not)

open System.IO 
let sounds = Directory.GetFiles(Directory.GetCurrentDirectory(), "*.*") 
             |> Array.fold (fun smap sound -> if sound.Contains(".ogg") || sound.ToLower().Contains(".wav") then
                                               let sname = Path.GetFileNameWithoutExtension sound
                                               let sdata = (new Sound(sound))
                                               if sname = "walk" then sdata.Volume <- 40
                                               Map.add sname sdata smap
                                              else smap) Map.empty

///////////////////////////////////////////////////////////

let doHit (surf:Surface) str p1 (p2:Hero) weapon = 
    let multiplier = match weapon with
                        | Arrow(el) -> sounds.["arrowlaunch"].Play() |> ignore;  if p2.WeakTo = el then 4 else 2
                        | EnchantedSword(el) ->  sounds.["sword2"].Play()  |> ignore; if p2.WeakTo = el then 6 else 4
                        | Sword -> 3
                        | Nothing -> 1
   
    let hit1 = (damage p1.Dexterity p1.Strength (float(dist p1.Position p2.Position)) p1.Equipped) * multiplier  
    if hit1 = 0 then sounds.["shieldhit"].Play() |> ignore 
    else      
        if hit1 / multiplier > 10 then
          surf.Blit(splat, point w h p2.Position )
          surf.Update()
          Threading.Thread.Sleep(200)
          sounds.["grunt"].Play() |> ignore
    printfn "%s Health: %d \n%s does %d damage to Enemy (%d/%d)" str p1.Health str hit1 p2.Health p2.MaxHealth     
    p2.Health <- p2.Health - hit1       



let validMove (curMap:Map<int*int, Item>) (sx,sy) (x,y) p2 dir = 
   if isBlocked curMap (x,y) p2 then
       let options = [for i in 0..3 -> let move = Move i in pseudoMove (sx,sy) move] 
                            |> List.map(fun pm -> pm, if isBlocked curMap pm p2 then max 0 (dir * 1000) else dir * dist pm p2)
       let checks = options |> List.sumBy snd
       if checks = 0 || checks = 5000 then None else options |> List.minBy snd |> fst |> Some 

   else Some (x,y)


let doAction (surf:Surface) (hero: Hero) (enemy:Hero) str action (curMap:Map<int*int,Item>) = 
  match action with
  | Move (d) -> let x,y = hero.Position
                sounds.["walk"].Play()  |> ignore
                hero.Position <- 
                    match d with 
                      | 0 -> printfn "%s moves left" str; (x - 1) |> wrap mapwidth, y
                      | 1 -> printfn "%s moves right" str; (x + 1) % mapwidth, y 
                      | 2 -> printfn "%s moves up" str;  x, (y - 1)  |> wrap mapheight
                      | _ -> printfn "%s moves down" str; x, (y + 1) % mapheight 
                curMap
  | PickUp -> if curMap.ContainsKey(hero.Position) then 
                match curMap.[hero.Position] with
                  | Weapon(wep) -> let dest = new Drawing.Point(600,400);  
                                   let actstr = sprintf "%s picks up %A" str wep          
                                   let surfVar = font.Render(actstr,Color.DarkBlue) 
                                   surf.Blit(surfVar)
                                   surf.Update()
                                   printfn "%s" actstr; hero.Equipped <- wep; 
                  | Potion heal -> sounds.["drink"].Play() |> ignore; 
                                   printfn "%s uses potion" str; 
                                   hero.Health <- (hero.Health + ((float hero.MaxHealth * heal) |> round |> int))
                  | _ -> ()
                curMap.Remove(hero.Position)
              else curMap
  | AttackMode ->  let hhealth = float hero.Health / float hero.MaxHealth 
                   match (dist hero.Position enemy.Position) , hero.Equipped with
                     | (d, Arrow(_) as bow) when d <= 10 && hhealth > 0.2 && lineofSight curMap hero.Position enemy.Position-> 
                          arrowAnim hero.Position enemy.Position
                          doHit surf str hero enemy (snd bow) 
                     | (d, weapon) when d <= 1 && (hhealth > 0.3) -> doHit surf str hero enemy weapon
                     | _ -> sounds.["walk"].Play()  |> ignore
                            let x,y = hero.Position 
                            let x2, y2 = enemy.Position
                            let m1 = random.Next(0,2)
                            let m2 = if m1 = 0 then 1 else random.Next(0,2)
                            let dir = if hhealth < 0.5 then -1 else 1
                            let dy = sign(y2 - y)
                            let dx = sign (x2 - x)//if dy = 0 && x = x2 then 1 else 
                            let x', y' = (x + dx * m1 * dir) % mapwidth |> wrap mapwidth, (y + dy * m2 * dir) % mapheight |> wrap mapheight
                            hero.Position <- match (validMove curMap hero.Position (x', y') enemy.Position dir) with | None -> x,y | Some(npos) -> npos
                   curMap // x = 5 x2 = 7 sign x2 - x, = 7 - 5 = 1
  | Freeze -> curMap

///////////////////////////////////////////////////////////

type Agent<'a> = MailboxProcessor<'a>

let generateHero () = 
  let hero =
   {Intelligence = (if random.NextDouble() < 0.1 then random.Next(70,100) else random.Next(10,70)) ; 
    Strength = random.Next(5,50); 
    MaxHealth = random.Next(200,500) ;  
    Health = 0;
    Dexterity = float(random.Next(10,100))
    WeakTo = Elements.[random.Next(0,3)]
    Equipped = Nothing
    Position = random.Next(0,mapwidth), random.Next(0,mapheight)}
  hero.Health <- hero.MaxHealth
  hero

let player1 = generateHero() 
let player2 = generateHero()

let map = [ for i in 0..44 do
             let p = random.NextDouble()             
             let item = if p <= 0.05 then 
                            Some (Weapon(EnchantedSword (Elements.[random.Next(0,3)])))//5%  
                        elif p <= 0.2 then
                            Some (Weapon (Arrow (Elements.[random.Next(0,3)]))) //15%
                        elif p <= 0.35 then
                            Some (Weapon (Sword))//15%
                        elif p <= 0.50 then
                             Some (Potion(random.NextDouble())) //15%
                        elif p <= 0.9 then Some(Tree) //40%
                        else None 
             let pos = (random.Next(0,mapwidth), random.Next(0, mapheight))
             if ((pos <> player1.Position && pos <> player2.Position) || item <> Some(Tree)) && item <> None then yield (pos, item.Value)]  
              |> Map.ofList
printfn "%A" map

type GameMsg = Tick of Surface | GetMap of AsyncReplyChannel<Map<int * int, Item>>

let gameAgent = 
 Agent.Start(fun inbox ->  
    let rec fight round (p1:Hero) p1Path p1Score (p2:Hero) p2Path p2Score (curMap) =  async {
       let! msg = inbox.Receive()
       match msg with 
        | Tick surf ->
          match (p1.Health, p2.Health) with 
           | (LessZero, AboveZero) -> printfn "player 2 wins"
           | (LessZero, LessZero) -> printfn "Both die"
           | (AboveZero, LessZero) -> printfn "player 1 wins" 
           | _ -> printfn "Thinking..."
                  printfn "Player1: \n%A \n\nPlayer 2\n %A\n" player1 player2
                  let p1path,p1score = if p1Path = [] then think Drawing.Color.OrangeRed surf curMap p1Path p1Score p1 p2 else p1Path, p1Score
                  let p2path,p2score = if p2Path = [] then think Drawing.Color.AliceBlue surf curMap p2Path p2Score p2 p1 else p2Path, p2Score
                  let act, p1Next = p1path.Head , p1path.Tail
                  let act2, p2Next = p2path.Head , p2path.Tail
                  let newMap = doAction surf p1 p2 "Player 1" act curMap |> doAction surf p2 p1 "Player 2" act2 
                  printfn "P1 %A \nP2%A" p1path p2path
          
                  printfn "%A" curMap
                  printfn "Round %d, Next" round
                //  Console.ReadLine() 
                  return! fight (round + 1) p1 p1Next p1score p2 p2Next p2score newMap
        | GetMap reply -> reply.Reply(curMap) ; return! fight round p1 p1Path p1Score p2 p2Path p2Score curMap}
    fight 0 player1 [] 0 player2 [] 0 map )
  
//////////////////////////////////////////////////////////////////////////

let potion = sprite "potion.bmp" white
let blood = sprite "blood.bmp" white
let swordflame = sprite "fsword.bmp" white
let swordice = sprite "swordice.bmp" white
let bow = sprite "bow.bmp" white
let sword = sprite "sword.bmp" white
let archers = [|sprite "archer1.bmp" white; sprite "archer2.bmp" white|]
let soldiers = [|sprite "soldier1.bmp" white;sprite "soldier2.bmp" white; sprite "soldier1Buffed.bmp" white ; sprite "soldier2Buffed.bmp" white|]
let tree = sprite "tree.bmp" None
Video.WindowCaption <- "AI Fight!"

let draw (x, y) (sprite:Sprites.Sprite) = 
  surface.Blit(sprite, Drawing.Point(x * grass.Width,y * grass.Height)) |> ignore 

let drawBar x health = 
  let w = max 0 ((health * 30.) |> round |> int)
  surface.Fill(rect x 10 w 10, Color.Red)

let drawgrass () =
    for y in 0..mapheight do
     for x in 0..mapwidth do
        surface.Blit(grass, Drawing.Point(x * grass.Width,y * grass.Height)) |> ignore

let drawSoldiers (p:Hero) (img: int) =
    let img = match p.Equipped with Arrow(_) -> archers.[img] | EnchantedSword(_) -> soldiers.[img + 2] | _ -> soldiers.[img]
    surface.Blit(img, (point grass.Width grass.Height p.Position)) |> ignore 

let drawMap lemap = 
    lemap |> Map.iter (fun (x,y) item -> match item with 
                                         | Potion _ -> surface.Blit(potion, point w h (x,y)) |> ignore
                                         | Weapon(Arrow(_)) -> surface.Blit(bow, point w h (x,y)) |> ignore
                                         | Weapon(Sword) -> surface.Blit(sword, point w h (x,y)) |> ignore
                                         | Weapon(EnchantedSword(Fire)) -> surface.Blit(swordflame, point w h (x,y)) |> ignore
                                         | Weapon(EnchantedSword(Lightning)) 
                                         | Weapon(EnchantedSword(Ice)) -> surface.Blit(swordice, point w h (x,y)) |> ignore
                                         | Tree -> surface.Blit(tree, point w h (x,y)) |> ignore
                                         | _ -> ()
                                          )
 
Events.Tick.Add(fun _ -> gameAgent.Post(Tick surface)
                         let newMap = gameAgent.PostAndReply(fun mapChannel -> GetMap mapChannel)
                         surface.Update() 
                         drawgrass()
                         drawMap newMap
                         drawBar 10 (float player1.Health/ float player1.MaxHealth)
                         drawBar (surface.Width - 40) (float player2.Health/ float player2.MaxHealth)
                         if player1.Health > 0 then drawSoldiers player1 0 else draw player1.Position blood
                         if player2.Health > 0 then drawSoldiers player2 1 else draw player2.Position blood
                         Thread.Sleep(50)
                         surface.Update())

Events.Quit.Add(fun q -> Events.QuitApplication())

drawgrass()
drawMap map
drawSoldiers player1 0
drawSoldiers player2 1  
surface.Update() 
Events.TargetFps <- 20
Events.Run()  
sounds |> Map.iter (fun _ s -> s.Close())
surface.Close()
Events.Close() 

