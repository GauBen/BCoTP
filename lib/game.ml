open Aliases

(* Repères géographiques *)
type coords = { x : int; overground : bool }

(* Ressources du joueur *)
type resource = { name : int; amount : float }

type resources = resource list

(* Bâtiments et sources de ressources *)
type level = {
  initialResources : resources;
  production : resources;
  (* En unités par minute *)
  storage : resources;
}

type structure = {
  levels : level array;
  width : int;
  editable : bool;
  overground : bool;
}

type worldStructure = {
  structure : structure;
  (* "Hérité" de structure *)
  mutable initialResources : resources;
  mutable production : resources;
  mutable storage : resources;
  width : int;
  editable : bool;
  (* / *)
  mutable coords : coords;
  player's : bool;
  mutable availableResources : resources;
  mutable level : int;
  mutable working : bool;
  mutable timeBeforeWorking : float;
}

type update = { production : resources; produced : resources }

type explored = No | Discovered | Yes

type state = Playing | Paused | GameOver | Won

type cell = {
  biome : int;
  x : int;
  mutable ground : worldStructure list;
  mutable explored : explored;
}

type interactions = {
  mutable selectionCoords : coords option;
  mutable selection : worldStructure list;
  mutable toBeBuilt : structure option;
  mutable possibleCoords : bool array;
}

(* Ville *)
type city = {
  width : int;
  (* Longueur de la ville *)
  cells : cell array;
  mutable storage : resources;
  (* Totaux des valeurs des bâtiments *)
  mutable production : resources;
  mutable availableResources : resources;
  mutable time : float;
  mutable state : state;
  interactions : interactions;
}

type game = { cities : city array }

(* Paramètres d'une planète *)
module Defaults = struct
  let planetWidth = 80
end

(* Ressources du jeu *)
module Resources = struct
  let villagers = 1

  let water = 2

  let food = 3

  let wood = 4

  let sand = 5

  let metal = 6

  let oil = 7

  let electricity = 8

  let length = 8 (* Nombre de ressources différentes *)
end

(* Environnements *)
module Biomes = struct
  let plain = 1

  let desert = 2

  let ocean = 4

  let mountain = 8
end

(* Caractéristiques des structures *)
module Structures = struct
  (* Ressources *)
  let forest =
    {
      levels =
        [|
          {
            initialResources = [ { name = Resources.wood; amount = 150. } ];
            production = [];
            storage = [];
          };
          {
            initialResources = [ { name = Resources.wood; amount = 300. } ];
            production = [];
            storage = [];
          };
          {
            initialResources = [ { name = Resources.wood; amount = 600. } ];
            production = [];
            storage = [];
          };
        |];
      width = 1;
      editable = false;
      overground = true;
    }

  let groundwater =
    {
      levels =
        [|
          {
            initialResources = [ { name = Resources.water; amount = 300. } ];
            production = [];
            storage = [];
          };
          {
            initialResources = [ { name = Resources.water; amount = 600. } ];
            production = [];
            storage = [];
          };
          {
            initialResources = [ { name = Resources.water; amount = 1200. } ];
            production = [];
            storage = [];
          };
        |];
      width = 1;
      editable = false;
      overground = false;
    }

  let seam =
    {
      levels =
        [|
          {
            initialResources = [ { name = Resources.metal; amount = 200. } ];
            production = [];
            storage = [];
          };
          {
            initialResources = [ { name = Resources.metal; amount = 400. } ];
            production = [];
            storage = [];
          };
          {
            initialResources = [ { name = Resources.metal; amount = 800. } ];
            production = [];
            storage = [];
          };
        |];
      width = 1;
      editable = false;
      overground = false;
    }

  let oilDeposit =
    {
      levels =
        [|
          {
            initialResources = [ { name = Resources.oil; amount = 200. } ];
            production = [];
            storage = [];
          };
          {
            initialResources = [ { name = Resources.oil; amount = 400. } ];
            production = [];
            storage = [];
          };
          {
            initialResources = [ { name = Resources.oil; amount = 800. } ];
            production = [];
            storage = [];
          };
        |];
      width = 1;
      editable = false;
      overground = false;
    }

  let field =
    {
      levels =
        [|
          {
            initialResources = [ { name = Resources.food; amount = 250. } ];
            production = [];
            storage = [];
          };
        |];
      width = 1;
      editable = true;
      overground = true;
    }

  (* Bâtiments *)
  let cityHall =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.villagers; amount = 5. };
                { name = Resources.water; amount = 60. };
                { name = Resources.food; amount = 51. };
                { name = Resources.wood; amount = 85. };
                { name = Resources.sand; amount = 30. };
                { name = Resources.metal; amount = 0. };
                { name = Resources.oil; amount = 0. };
                { name = Resources.electricity; amount = 0. };
              ];
            production =
              [
                { name = Resources.water; amount = -0.25 };
                { name = Resources.food; amount = -0.16667 };
              ];
            storage =
              [
                { name = Resources.villagers; amount = 5. };
                { name = Resources.water; amount = 120. };
                { name = Resources.food; amount = 100. };
                { name = Resources.wood; amount = 250. };
                { name = Resources.sand; amount = 100. };
                { name = Resources.metal; amount = 0. };
                { name = Resources.oil; amount = 0. };
                { name = Resources.electricity; amount = 0. };
              ];
          };
        |];
      width = 2;
      editable = false;
      overground = true;
    }

  let house =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.villagers; amount = 5. };
                { name = Resources.wood; amount = -40. };
              ];
            production =
              [
                { name = Resources.water; amount = -0.25 };
                { name = Resources.food; amount = -0.16667 };
              ];
            storage =
              [
                { name = Resources.villagers; amount = 5. };
                { name = Resources.water; amount = 40. };
                { name = Resources.food; amount = 30. };
              ];
          };
        |];
      width = 1;
      editable = true;
      overground = true;
    }

  let building =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.villagers; amount = 20. };
                { name = Resources.wood; amount = -80. };
                { name = Resources.sand; amount = -300. };
                { name = Resources.metal; amount = -50. };
              ];
            production =
              [
                { name = Resources.water; amount = -1. };
                { name = Resources.food; amount = -0.66667 };
              ];
            storage =
              [
                { name = Resources.villagers; amount = 20. };
                { name = Resources.water; amount = 100. };
                { name = Resources.food; amount = 100. };
              ];
          };
          {
            initialResources =
              [
                { name = Resources.villagers; amount = 10. };
                { name = Resources.wood; amount = -40. };
                { name = Resources.sand; amount = -150. };
                { name = Resources.metal; amount = -50. };
              ];
            production =
              [
                { name = Resources.water; amount = -1.5 };
                { name = Resources.food; amount = -1. };
              ];
            storage =
              [
                { name = Resources.villagers; amount = 30. };
                { name = Resources.water; amount = 150. };
                { name = Resources.food; amount = 150. };
              ];
          };
          {
            initialResources =
              [
                { name = Resources.villagers; amount = 10. };
                { name = Resources.sand; amount = -100. };
                { name = Resources.metal; amount = -25. };
              ];
            production =
              [
                { name = Resources.water; amount = -2. };
                { name = Resources.food; amount = -1.33334 };
              ];
            storage =
              [
                { name = Resources.villagers; amount = 40. };
                { name = Resources.water; amount = 300. };
                { name = Resources.food; amount = 300. };
              ];
          };
        |];
      width = 2;
      editable = true;
      overground = true;
    }

  let well =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.wood; amount = -50. };
                { name = Resources.sand; amount = -50. };
              ];
            production = [ { name = Resources.water; amount = 0.5 } ];
            storage = [ { name = Resources.water; amount = 100. } ];
          };
          {
            initialResources =
              [
                { name = Resources.villagers; amount = -1. };
                { name = Resources.sand; amount = -200. };
                { name = Resources.metal; amount = -50. };
              ];
            production = [ { name = Resources.water; amount = 1.5 } ];
            storage = [ { name = Resources.water; amount = 500. } ];
          };
        |];
      width = 1;
      editable = true;
      overground = true;
    }

  let derrick =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.wood; amount = -50. };
                { name = Resources.sand; amount = -50. };
                { name = Resources.metal; amount = -100. };
              ];
            production = [ { name = Resources.oil; amount = 0.5 } ];
            storage = [ { name = Resources.oil; amount = 150. } ];
          };
        |];
      width = 1;
      editable = true;
      overground = true;
    }

  let tunnel =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.wood; amount = -50. };
                { name = Resources.sand; amount = -100. };
                { name = Resources.metal; amount = -25. };
              ];
            production = [];
            storage = [];
          };
        |];
      width = 1;
      editable = true;
      overground = true;
    }

  let coalFiredPlant =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.villagers; amount = -5. };
                { name = Resources.sand; amount = -100. };
                { name = Resources.metal; amount = -120. };
              ];
            production =
              [
                { name = Resources.wood; amount = -0.25 };
                { name = Resources.electricity; amount = 3. };
              ];
            storage = [ { name = Resources.electricity; amount = 20. } ];
          };
        |];
      width = 1;
      editable = true;
      overground = true;
    }

  let crops =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.villagers; amount = -1. };
                { name = Resources.wood; amount = -25. };
              ];
            production =
              [
                { name = Resources.water; amount = -0.01667 };
                { name = Resources.food; amount = 0.66667 };
              ];
            storage = [];
          };
        |];
      width = 1;
      editable = true;
      overground = true;
    }

  let farm =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.villagers; amount = -2. };
                { name = Resources.wood; amount = -125. };
              ];
            production =
              [
                { name = Resources.water; amount = -0.08333 };
                { name = Resources.food; amount = 1.5 };
              ];
            storage = [ { name = Resources.food; amount = 200. } ];
          };
        |];
      width = 1;
      editable = true;
      overground = true;
    }

  let lumberCamp =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.villagers; amount = -1. };
                { name = Resources.wood; amount = -40. };
              ];
            production = [ { name = Resources.wood; amount = 0.5 } ];
            storage = [ { name = Resources.wood; amount = 200. } ];
          };
          {
            initialResources = [ { name = Resources.villagers; amount = -1. } ];
            production = [ { name = Resources.wood; amount = 1. } ];
            storage = [ { name = Resources.wood; amount = 200. } ];
          };
          {
            initialResources = [ { name = Resources.villagers; amount = -1. } ];
            production = [ { name = Resources.wood; amount = 1.5 } ];
            storage = [ { name = Resources.wood; amount = 200. } ];
          };
        |];
      width = 1;
      editable = true;
      overground = true;
    }

  let minerCamp =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.villagers; amount = -1. };
                { name = Resources.wood; amount = -40. };
              ];
            production =
              [
                { name = Resources.metal; amount = 0.16667 };
                { name = Resources.sand; amount = 0.5 };
              ];
            storage =
              [
                { name = Resources.sand; amount = 200. };
                { name = Resources.metal; amount = 100. };
              ];
          };
          {
            initialResources = [ { name = Resources.villagers; amount = -1. } ];
            production =
              [
                { name = Resources.metal; amount = 0.3334 };
                { name = Resources.sand; amount = 1. };
              ];
            storage =
              [
                { name = Resources.sand; amount = 200. };
                { name = Resources.metal; amount = 100. };
              ];
          };
          {
            initialResources = [ { name = Resources.villagers; amount = -1. } ];
            production =
              [
                { name = Resources.metal; amount = 0.5 };
                { name = Resources.sand; amount = 1.5 };
              ];
            storage =
              [
                { name = Resources.sand; amount = 200. };
                { name = Resources.metal; amount = 100. };
              ];
          };
        |];
      width = 1;
      editable = true;
      overground = true;
    }

  let harbor =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.villagers; amount = -2. };
                { name = Resources.wood; amount = -120. };
              ];
            production = [];
            storage = [];
          };
        |];
      width = 1;
      editable = true;
      overground = true;
    }

  let boat =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.villagers; amount = -2. };
                { name = Resources.wood; amount = -150. };
              ];
            production = [];
            storage = [];
          };
        |];
      width = 1;
      editable = true;
      overground = true;
    }

  let university =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.villagers; amount = -15. };
                { name = Resources.wood; amount = -200. };
                { name = Resources.sand; amount = -500. };
              ];
            production =
              [
                { name = Resources.food; amount = -0.5 };
                { name = Resources.electricity; amount = -3. };
              ];
            storage = [];
          };
        |];
      width = 2;
      editable = true;
      overground = true;
    }

  let rocketStation =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.villagers; amount = -20. };
                { name = Resources.sand; amount = -500. };
                { name = Resources.metal; amount = -500. };
              ];
            production = [ { name = Resources.electricity; amount = -3. } ];
            storage = [ { name = Resources.oil; amount = 500. } ];
          };
          {
            initialResources =
              [
                { name = Resources.metal; amount = -500. };
                { name = Resources.oil; amount = -500. };
              ];
            production = [ { name = Resources.electricity; amount = -1. } ];
            storage = [ { name = Resources.oil; amount = 500. } ];
          };
          {
            initialResources = [];
            production = [ { name = Resources.electricity; amount = -1. } ];
            storage = [ { name = Resources.oil; amount = 500. } ];
          };
        |];
      width = 2;
      editable = true;
      overground = true;
    }

  let windTurbine =
    {
      levels =
        [|
          {
            initialResources =
              [
                { name = Resources.sand; amount = -50. };
                { name = Resources.metal; amount = -100. };
              ];
            production = [ { name = Resources.electricity; amount = 1. } ];
            storage = [ { name = Resources.electricity; amount = 10. } ];
          };
        |];
      width = 1;
      editable = true;
      overground = true;
    }
end

(* / Structures *)

(* Calculs sur les ressources *)
module Calculations = struct
  let sum_resources listOfResources =
    let sum = Array.make Resources.length 0. in
    List.iter
      (fun ressources ->
        List.iter
          (fun r -> sum.(r.name - 1) <- sum.(r.name - 1) +. r.amount)
          ressources)
      listOfResources;
    Array.to_list (Array.mapi (fun i amount -> { name = i + 1; amount }) sum)

  let resources_produced production time =
    List.map
      (fun res -> { name = res.name; amount = res.amount *. time /. 60. })
      production

  let get_resource resources name =
    match List.find_opt (fun r -> name = r.name) resources with
    | Some r -> r.amount
    | None -> 0.

  let max_resource r1 r2 name =
    max (get_resource r1 name) (get_resource r2 name)

  let min_resource r1 r2 name =
    min (get_resource r1 name) (get_resource r2 name)
end

(* / Calculations *)

(* Gestion de la ville *)
module City = struct
  let game_over city = city.state <- GameOver

  let you_win city = city.state <- Won

  let new_worldStructure (structure, level) x player's tBW =
    let s = structure.levels.(level) in
    let wS =
      {
        structure;
        initialResources = [];
        production = [];
        storage = [];
        width = structure.width;
        editable = structure.editable;
        coords = { x; overground = structure.overground };
        player's;
        availableResources = [];
        level;
        working = tBW <= 0.;
        timeBeforeWorking = tBW;
      }
    in
    if structure = Structures.cityHall then
      wS.initialResources <-
        [
          {
            name = Resources.villagers;
            amount =
              Calculations.get_resource s.initialResources Resources.villagers;
          };
        ]
    else wS.initialResources <- s.initialResources;
    wS.storage <- s.storage;
    if not wS.player's then wS.availableResources <- s.initialResources;
    wS

  (* Temps où pendant lequel des ressources sont produites *)
  let production_time city production availableResources time opposite =
    List.fold_left min time
      (List.map
         (fun prod ->
           (if opposite then -1. else 1.)
           *. ((if prod.amount < 0. <> opposite then 0.
               else Calculations.get_resource city.storage prod.name)
              -. Calculations.get_resource availableResources prod.name)
           /. prod.amount)
         production)

  let find_worldStructure city structure x offsets =
    List.find_opt
      (fun wS -> wS.structure = structure)
      (List.concat
         (List.map (fun i -> (get city.cells (x + i)).ground) offsets))

  (* Clique sur un bâtiment *)
  let select city { x; overground } =
    if (get city.cells x).explored <> Yes then city.interactions.selection <- []
    else (
      city.interactions.selectionCoords <-
        Some { x = modpos x city.width; overground };
      city.interactions.selection <-
        List.concat
          [
            List.find_all
              (fun s -> s.coords.overground = overground && s.width = 2)
              (get city.cells (x - 1)).ground;
            List.find_all
              (fun s -> s.coords.overground = overground)
              (get city.cells x).ground;
          ])

  (* Calcul de la production d'un bâtiment *)
  let update_worldStructure city (wS : worldStructure) time =
    let level = wS.structure.levels.(wS.level) in
    let prod = level.production in
    match wS.structure with
    | i
      when i = Structures.well || i = Structures.derrick
           || i = Structures.lumberCamp -> (
        let wSo =
          match i with
          | i when i = Structures.well ->
              find_worldStructure city Structures.groundwater wS.coords.x [ 0 ]
          | i when i = Structures.derrick ->
              find_worldStructure city Structures.oilDeposit wS.coords.x [ 0 ]
          | i when i = Structures.lumberCamp ->
              find_worldStructure city Structures.forest wS.coords.x
                [ -1; 1; -2; 2; -3; 3 ]
          | _ -> failwith "ah."
        in
        match wSo with
        | Some deposit ->
            let dt =
              min
                (production_time city prod deposit.availableResources time true)
                (production_time city prod city.availableResources time false)
            in
            wS.working <- not (dt = 0.);
            let produced =
              List.map
                (fun res -> { name = res.name; amount = res.amount *. dt })
                prod
            in
            deposit.availableResources <-
              List.map
                (fun res ->
                  {
                    name = res.name;
                    amount =
                      Calculations.get_resource deposit.availableResources
                        res.name
                      -. res.amount;
                  })
                produced;
            wS.production <-
              (if wS.working then prod
              else List.map (fun r -> { name = r.name; amount = 0. }) prod);
            { production = wS.production; produced }
        | _ ->
            wS.working <- false;
            {
              production =
                List.map (fun r -> { name = r.name; amount = 0. }) prod;
              produced = [];
            })
    | i when i = Structures.crops -> (
        let wSo = find_worldStructure city Structures.field wS.coords.x [ 0 ] in
        match wSo with
        | Some deposit ->
            let dt =
              min
                (production_time city prod deposit.availableResources time true)
                (production_time city prod city.availableResources time false)
            in
            wS.working <- not (dt = 0.);
            let produced =
              List.map
                (fun res -> { name = res.name; amount = res.amount *. dt })
                prod
            in
            deposit.availableResources <-
              [
                {
                  name = Resources.food;
                  amount =
                    Calculations.get_resource deposit.availableResources
                      Resources.food
                    -. Calculations.get_resource produced Resources.food;
                };
              ];
            wS.production <-
              (if wS.working then prod
              else List.map (fun r -> { name = r.name; amount = 0. }) prod);
            { production = wS.production; produced }
        | _ ->
            wS.working <- false;
            {
              production =
                List.map (fun r -> { name = r.name; amount = 0. }) prod;
              produced = [];
            })
    | i when i = Structures.minerCamp -> (
        let wSo = find_worldStructure city Structures.seam wS.coords.x [ 0 ] in
        let prodSand =
          [
            {
              name = Resources.sand;
              amount = Calculations.get_resource prod Resources.sand;
            };
          ]
        in
        let dtS =
          production_time city prodSand city.availableResources time false
        in
        wS.working <- not (dtS = 0.);
        let producedSand =
          if not wS.working then []
          else
            List.map
              (fun res -> { name = res.name; amount = res.amount *. dtS })
              prodSand
        in
        wS.production <-
          (if dtS = 0. then
           List.map (fun r -> { name = r.name; amount = 0. }) prodSand
          else prodSand);
        match wSo with
        | Some seam ->
            let prodMetal =
              [
                {
                  name = Resources.metal;
                  amount = Calculations.get_resource prod Resources.metal;
                };
              ]
            in
            let dtM =
              min
                (production_time city prodMetal seam.availableResources time
                   true)
                (production_time city prodMetal city.availableResources time
                   false)
            in
            let producedMetal =
              List.map
                (fun res -> { name = res.name; amount = res.amount *. dtM })
                prodMetal
            in
            seam.availableResources <-
              List.map
                (fun res ->
                  {
                    name = res.name;
                    amount =
                      Calculations.get_resource seam.availableResources res.name
                      -. res.amount;
                  })
                producedMetal;
            wS.working <- wS.working || not (dtM = 0.);
            wS.production <-
              List.concat
                [
                  wS.production;
                  (if not (dtM = 0.) then prodMetal
                  else
                    List.map (fun r -> { name = r.name; amount = 0. }) prodMetal);
                ];
            {
              production = wS.production;
              produced = List.concat [ producedSand; producedMetal ];
            }
        | _ -> { production = wS.production; produced = producedSand })
    | _ ->
        let dt = production_time city prod city.availableResources time false in
        wS.working <- not (dt = 0.);
        let produced =
          if not wS.working then []
          else
            List.map
              (fun res -> { name = res.name; amount = res.amount *. dt })
              prod
        in
        wS.production <-
          (if wS.working then level.production
          else List.map (fun r -> { name = r.name; amount = 0. }) prod);
        { production = wS.production; produced }

  let can_build city structure x =
    let pc = get city.cells (x - 1) in
    let c = get city.cells x in
    let nc = get city.cells (x + 1) in
    let o = structure.overground in
    (* Partie découverte ? *)
    c.explored = Yes
    && ((* Biome constructible ? *)
        c.biome <> Biomes.ocean && c.biome <> Biomes.mountain
        && structure <> Structures.crops
        && structure <> Structures.harbor
        && structure <> Structures.boat
        && structure <> Structures.tunnel
       || (c.biome = Biomes.plain && structure = Structures.crops)
       || c.biome = Biomes.ocean
          && structure = Structures.harbor
          && (pc.biome <> Biomes.ocean || nc.biome <> Biomes.ocean)
       || c.biome = Biomes.mountain && false
          && (pc.biome <> Biomes.mountain || nc.biome <> Biomes.mountain)
       || (c.biome = Biomes.ocean && structure = Structures.boat)
       || (c.biome = Biomes.mountain && structure = Structures.tunnel))
    && (structure.width = 1
       || (nc.biome <> Biomes.mountain && nc.biome <> Biomes.ocean))
    && List.for_all
         (fun a -> a) (* Autres bâtiments qui gênent ? *)
         (List.init structure.width (fun i ->
              List.for_all
                (function
                  | { coords = { overground; _ }; _ } when overground = o ->
                      false
                  | _ -> true)
                (get city.cells (x + i)).ground))
    && List.for_all
         (function
           | { coords = { overground; _ }; width = 2; _ } when overground = o ->
               false
           | _ -> true)
         pc.ground

  let can_buy city (structure, level) =
    let l = structure.levels.(level) in
    List.for_all
      (fun res -> res.amount >= 0.)
      (Calculations.sum_resources
         [ city.availableResources; l.initialResources ])
    &&
    match structure with
    | i when i = Structures.boat ->
        Array.exists
          (fun c ->
            List.exists (fun wS -> wS.structure = Structures.harbor) c.ground)
          city.cells
    | i when i = Structures.rocketStation || i = Structures.windTurbine ->
        Array.exists
          (fun c ->
            List.exists
              (fun wS -> wS.structure = Structures.university && wS.working)
              c.ground)
          city.cells
    | _ -> true

  let update_storage city =
    city.storage <- [];
    Array.iter
      (fun cell ->
        List.iter
          (fun wS ->
            if wS.player's then
              city.storage <-
                Calculations.sum_resources [ city.storage; wS.storage ])
          cell.ground)
      city.cells;
    city.availableResources <-
      List.map
        (fun r ->
          {
            r with
            amount =
              min r.amount (Calculations.get_resource city.storage r.name);
          })
        city.availableResources

  let explore city x =
    let vision, range =
      if (get city.cells x).biome = Biomes.mountain then ([ -2; -1; 1; 2 ], 2)
      else ([ -4; -3; -2; -1; 1; 2; 3; 4 ], 4)
    in
    List.iter
      (fun i ->
        let cell = get city.cells (x + i) in
        cell.explored <-
          (if abs i = range then max Discovered cell.explored
          else if
          (cell.biome = Biomes.mountain && abs i = 1)
          || cell.biome <> Biomes.mountain
         then Yes
          else max Discovered cell.explored))
      vision

  let upgrade city wS =
    if
      wS.level < last wS.structure.levels
      && can_buy city (wS.structure, wS.level + 1)
    then (
      let l = wS.structure.levels.(wS.level + 1) in
      city.availableResources <-
        Calculations.sum_resources
          [ city.availableResources; l.initialResources ];
      wS.initialResources <-
        Calculations.sum_resources [ wS.initialResources; l.initialResources ];
      wS.level <- wS.level + 1;
      wS.storage <- l.storage;
      update_storage city;
      if wS.structure = Structures.rocketStation && wS.level = 2 then
        you_win city);
    if
      wS.structure = Structures.boat
      && can_build city Structures.boat (wS.coords.x - 1)
    then (
      let pcell = get city.cells wS.coords.x in
      pcell.ground <- List.filter (fun i -> i <> wS) pcell.ground;
      let cell = get city.cells (wS.coords.x - 1) in
      cell.ground <- wS :: cell.ground;
      wS.coords <-
        {
          x = modpos (wS.coords.x - 1) city.width;
          overground = wS.coords.overground;
        };
      explore city wS.coords.x)

  let downgrade city wS =
    if wS.level > 0 then (
      let res =
        List.map
          (fun r -> { r with amount = -.r.amount })
          (List.filter
             (fun r -> r.name = Resources.villagers)
             wS.structure.levels.(wS.level).initialResources)
      in
      city.availableResources <-
        Calculations.sum_resources [ city.availableResources; res ];
      wS.initialResources <-
        Calculations.sum_resources [ wS.initialResources; res ];
      wS.level <- wS.level - 1;
      let l = wS.structure.levels.(wS.level) in
      wS.storage <- l.storage;
      update_storage city);
    if
      wS.structure = Structures.boat
      && can_build city Structures.boat (wS.coords.x + 1)
    then (
      let pcell = get city.cells wS.coords.x in
      pcell.ground <- List.filter (fun i -> i <> wS) pcell.ground;
      let cell = get city.cells (wS.coords.x + 1) in
      cell.ground <- wS :: cell.ground;
      wS.coords <-
        {
          x = modpos (wS.coords.x + 1) city.width;
          overground = wS.coords.overground;
        };
      explore city wS.coords.x)

  let build city (structure, level) x =
    if can_buy city (structure, level) && can_build city structure x then (
      let l = structure.levels.(level) in
      city.availableResources <-
        Calculations.sum_resources
          [ city.availableResources; l.initialResources ];
      let cell = get city.cells x in
      (if structure = Structures.crops then
       let field = new_worldStructure (Structures.field, 0) x false 0. in
       cell.ground <- field :: cell.ground);
      cell.ground <-
        new_worldStructure (structure, level) x true 0. :: cell.ground;
      update_storage city;
      explore city x)

  let destroy city wS =
    let cell = get city.cells wS.coords.x in
    cell.ground <-
      List.filter
        (fun i ->
          (not (i = wS))
          || (wS.structure = Structures.crops && i.structure = Structures.field))
        cell.ground;
    if wS.player's then (
      city.availableResources <-
        Calculations.sum_resources
          [
            city.availableResources;
            List.map
              (fun r -> { r with amount = -.r.amount })
              (List.filter
                 (fun r -> r.amount > 0. || r.name = Resources.villagers)
                 wS.initialResources);
          ];
      update_storage city);
    match city.interactions.toBeBuilt with
    | Some s ->
        city.interactions.possibleCoords <-
          Array.mapi (fun x _ -> can_build city s x) city.cells
    | _ -> ()

  (* Génération du monde *)
  let deposit (structure, level) x =
    new_worldStructure (structure, level) x false 0.

  let level x w = min 2 (6 * ((w / 2) - abs (x - (w / 2))) / w)

  let wood x w = deposit (Structures.forest, level x w) x

  let water x w = deposit (Structures.groundwater, level x w) x

  let metal x w = deposit (Structures.seam, level x w) x

  let oil x w = deposit (Structures.oilDeposit, level x w) x

  let generate_cells width =
    let pi = acos (-1.) in
    let w = pi *. 1. /. 40. in
    let h x = 1.42 +. (0.6 *. sin (foi x /. w)) in
    Array.mapi
      (fun x biome ->
        let ground = ref [] in
        if biome = Biomes.plain && x mod 10 >= 5 && x mod 10 <= 7 then
          ground := wood x width :: !ground;
        if biome = Biomes.plain && x mod 4 = 2 then
          ground := water x width :: !ground;
        if biome = Biomes.plain && x mod 8 = 3 then
          ground := metal x width :: !ground;
        if biome = Biomes.desert && x mod 3 = 0 then
          ground := metal x width :: !ground;
        if biome = Biomes.desert && x mod 5 = 0 && x mod 3 <> 0 then
          ground := oil x width :: !ground;
        if x = 0 then
          ground :=
            new_worldStructure (Structures.cityHall, 0) x true 0. :: !ground;
        if x = 2 then
          ground := new_worldStructure (Structures.well, 0) x true 0. :: !ground;
        {
          biome;
          ground = !ground;
          x;
          explored =
            (if abs ((width / 2) + 1 - x) >= (width / 2) - 3 then Yes
            else if abs ((width / 2) + 1 - x) >= (width / 2) - 4 then Discovered
            else No);
        })
      (Array.init width (fun x ->
           match iof (h x) with
           | 0 -> Biomes.ocean
           | 2 -> Biomes.mountain
           | _ when x mod 12 >= 8 -> Biomes.desert
           | _ -> Biomes.plain))

  let new_city () =
    {
      width = Defaults.planetWidth;
      cells = generate_cells Defaults.planetWidth;
      storage =
        Calculations.sum_resources
          [
            Structures.cityHall.levels.(0).storage;
            Structures.well.levels.(0).storage;
          ];
      availableResources = Structures.cityHall.levels.(0).initialResources;
      production = [];
      time = 0.;
      state = Paused;
      interactions =
        {
          selectionCoords = None;
          selection = [];
          toBeBuilt = None;
          possibleCoords = [||];
        };
    }

  let update_city city timeSinceLastUpdate =
    city.time <- city.time +. timeSinceLastUpdate;
    city.production <- [];
    Array.iter
      (fun cell ->
        List.iter
          (fun wS ->
            if wS.player's then (
              let update = update_worldStructure city wS timeSinceLastUpdate in
              city.production <-
                Calculations.sum_resources
                  [ city.production; update.production ];
              city.availableResources <-
                Calculations.sum_resources
                  [ city.availableResources; update.produced ])
            else if List.for_all (fun r -> r.amount <= 0.) wS.availableResources
            then destroy city wS;
            if
              Calculations.get_resource city.availableResources Resources.water
              <= 0.
              || Calculations.get_resource city.availableResources
                   Resources.food
                 <= 0.
            then game_over city)
          cell.ground)
      city.cells;
    city.interactions.toBeBuilt <-
      (match city.interactions.toBeBuilt with
      | Some s when can_buy city (s, 0) -> Some s
      | _ -> None)
end

(* / City *)

(* Interactions avec l'interface graphique *)
module Interactions = struct
  let buildingList =
    [
      Structures.house; Structures.crops; Structures.well; Structures.minerCamp;
      Structures.lumberCamp; Structures.building; Structures.harbor;
      Structures.farm; Structures.derrick; Structures.university;
      Structures.rocketStation; Structures.boat; Structures.tunnel;
      Structures.coalFiredPlant; Structures.windTurbine;
    ]

  let want_to_build city s =
    city.interactions.toBeBuilt <-
      (if City.can_buy city (s, 0) then Some s else None);
    city.interactions.possibleCoords <-
      Array.mapi (fun x _ -> City.can_build city s x) city.cells

  let can_buy city structure = City.can_buy city (structure, 0)

  let click city { x; overground } =
    match city.interactions.toBeBuilt with
    | Some s when s.overground = overground ->
        City.build city (s, 0) x;
        city.interactions.toBeBuilt <- None
    | _ -> City.select city { x; overground }

  let show_delete_button city =
    List.exists (fun wS -> wS.player's) city.interactions.selection

  let can_delete city =
    List.exists
      (fun wS ->
        wS.editable
        && List.for_all
             (fun res -> res.amount >= 0.)
             (Calculations.sum_resources
                [
                  city.availableResources;
                  List.map
                    (fun r -> { r with amount = -.r.amount })
                    (List.filter (fun r -> r.amount > 0.) wS.initialResources);
                ]))
      city.interactions.selection

  let show_up_button city =
    List.exists
      (fun wS ->
        (wS.editable && wS.level < last wS.structure.levels)
        || wS.structure = Structures.boat)
      city.interactions.selection

  let can_up city =
    List.exists
      (fun wS ->
        wS.editable && wS.working
        && wS.level < last wS.structure.levels
        && City.can_buy city (wS.structure, wS.level + 1)
        || wS.structure = Structures.boat)
      city.interactions.selection

  let show_down_button city =
    List.exists
      (fun wS ->
        (wS.editable && wS.level > 0) || wS.structure = Structures.boat)
      city.interactions.selection

  let show_move_button city =
    List.exists
      (fun wS -> wS.structure = Structures.boat)
      city.interactions.selection

  let can_down city = show_down_button city

  let destroy city =
    if can_delete city then (
      List.iter (fun wS -> City.destroy city wS) city.interactions.selection;
      city.interactions.selection <- [])

  let earned_on_destroyed city =
    List.map
      (fun r -> { r with amount = -.r.amount })
      (List.filter
         (fun r -> r.amount > 0. || r.name = Resources.villagers)
         (List.hd city.interactions.selection).initialResources)

  let earned_on_downgrade city =
    let wS = List.hd city.interactions.selection in
    List.map
      (fun r -> { r with amount = -.r.amount })
      (List.filter
         (fun r -> r.name = Resources.villagers)
         wS.structure.levels.(wS.level).initialResources)

  let up_price city =
    let wS = List.nth city.interactions.selection 0 in
    wS.structure.levels.(wS.level + 1).initialResources

  let up city =
    if can_up city then City.upgrade city (List.hd city.interactions.selection)

  let down city =
    if can_down city then
      City.downgrade city (List.hd city.interactions.selection)

  let nothing city =
    city.interactions.selection <- [];
    city.interactions.selectionCoords <- None;
    city.interactions.toBeBuilt <- None
end

(* / Interactions *)

let new_game () = { cities = [| City.new_city () |] }

let update game timeSinceLastUpdate =
  for i = 0 to last game.cities do
    let city = game.cities.(i) in
    if city.state = Playing then City.update_city city timeSinceLastUpdate
  done
