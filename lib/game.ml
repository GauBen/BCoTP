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

  let length = 8

  let is_not_producible r = r = villagers || r = electricity
end

(* Environnements *)
module Biomes = struct
  let plain = 1

  let desert = 2

  let ocean = 4

  let mountain = 8

  let is_flat b = b = plain || b = desert
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
            initialResources = [ { name = Resources.wood; amount = 200. } ];
            production = [];
            storage = [];
          };
          {
            initialResources = [ { name = Resources.wood; amount = 250. } ];
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
            initialResources = [ { name = Resources.water; amount = 250. } ];
            production = [];
            storage = [];
          };
          {
            initialResources = [ { name = Resources.water; amount = 500. } ];
            production = [];
            storage = [];
          };
          {
            initialResources = [ { name = Resources.water; amount = 1000. } ];
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
            initialResources = [ { name = Resources.metal; amount = 300. } ];
            production = [];
            storage = [];
          };
          {
            initialResources = [ { name = Resources.metal; amount = 400. } ];
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
            initialResources = [ { name = Resources.oil; amount = 250. } ];
            production = [];
            storage = [];
          };
          {
            initialResources = [ { name = Resources.oil; amount = 300. } ];
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
                { name = Resources.water; amount = 30. };
                { name = Resources.food; amount = 21. };
                { name = Resources.wood; amount = 100. };
                { name = Resources.sand; amount = 50. };
                { name = Resources.metal; amount = 0. };
                { name = Resources.oil; amount = 0. };
                { name = Resources.electricity; amount = 0. };
              ];
            production =
              [
                { name = Resources.water; amount = -0.25 };
                { name = Resources.food; amount = -1. /. 6. };
              ];
            storage =
              [
                { name = Resources.villagers; amount = 5. };
                { name = Resources.water; amount = 50. };
                { name = Resources.food; amount = 100. };
                { name = Resources.wood; amount = 300. };
                { name = Resources.sand; amount = 150. };
                { name = Resources.metal; amount = 100. };
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
                { name = Resources.food; amount = -1. /. 6. };
              ];
            storage =
              [
                { name = Resources.villagers; amount = 5. };
                { name = Resources.water; amount = 50. };
                { name = Resources.food; amount = 50. };
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
                { name = Resources.sand; amount = -300. };
                { name = Resources.metal; amount = -50. };
              ];
            production =
              [
                { name = Resources.water; amount = -1. };
                { name = Resources.food; amount = -2. /. 3. };
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
                { name = Resources.sand; amount = -100. };
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
                { name = Resources.food; amount = -4. /. 3. };
              ];
            storage =
              [
                { name = Resources.villagers; amount = 40. };
                { name = Resources.water; amount = 200. };
                { name = Resources.food; amount = 200. };
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
                { name = Resources.wood; amount = -10. };
                { name = Resources.sand; amount = -40. };
              ];
            production = [ { name = Resources.water; amount = 0.5 } ];
            storage = [ { name = Resources.water; amount = 50. } ];
          };
          {
            initialResources =
              [
                { name = Resources.sand; amount = -150. };
                { name = Resources.metal; amount = -25. };
              ];
            production = [ { name = Resources.water; amount = 1.5 } ];
            storage = [ { name = Resources.water; amount = 250. } ];
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
                { name = Resources.sand; amount = -100. };
                { name = Resources.metal; amount = -75. };
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
                { name = Resources.sand; amount = -150. };
                { name = Resources.metal; amount = -100. };
                { name = Resources.electricity; amount = 50. };
              ];
            production = [ { name = Resources.wood; amount = -0.25 } ];
            storage = [ { name = Resources.electricity; amount = 50. } ];
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
                { name = Resources.water; amount = -5. };
                { name = Resources.wood; amount = -20. };
              ];
            production = [ { name = Resources.food; amount = 2. /. 3. } ];
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
                { name = Resources.villagers; amount = -3. };
                { name = Resources.wood; amount = -175. };
              ];
            production =
              [
                { name = Resources.water; amount = -1. /. 12. };
                { name = Resources.food; amount = 5. /. 3. };
              ];
            storage = [ { name = Resources.food; amount = 200. } ];
          };
        |];
      width = 2;
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
            storage = [ { name = Resources.wood; amount = 150. } ];
          };
          {
            initialResources = [ { name = Resources.villagers; amount = -1. } ];
            production = [ { name = Resources.wood; amount = 1. } ];
            storage = [ { name = Resources.wood; amount = 150. } ];
          };
          {
            initialResources = [ { name = Resources.villagers; amount = -1. } ];
            production = [ { name = Resources.wood; amount = 1.5 } ];
            storage = [ { name = Resources.wood; amount = 150. } ];
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
                { name = Resources.metal; amount = 1. /. 6. };
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
                { name = Resources.metal; amount = 1. /. 3. };
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
                { name = Resources.villagers; amount = -1. };
                { name = Resources.wood; amount = -100. };
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
                { name = Resources.wood; amount = -75. };
              ];
            production = [];
            storage = [];
          }; { initialResources = []; production = []; storage = [] };
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
                { name = Resources.sand; amount = -400. };
                { name = Resources.electricity; amount = -50. };
              ];
            production = [ { name = Resources.food; amount = -0.5 } ];
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
                { name = Resources.electricity; amount = -50. };
              ];
            production = [];
            storage = [ { name = Resources.oil; amount = 500. } ];
          };
          {
            initialResources =
              [
                { name = Resources.metal; amount = -250. };
                { name = Resources.oil; amount = -500. };
              ];
            production = [];
            storage = [ { name = Resources.oil; amount = 500. } ];
          };
          {
            initialResources =
              [ { name = Resources.electricity; amount = -25. } ];
            production = [];
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
                { name = Resources.metal; amount = -50. };
                { name = Resources.electricity; amount = 25. };
              ];
            production = [];
            storage = [ { name = Resources.electricity; amount = 25. } ];
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

  let is_cell_empty city (structure : structure) x =
    List.for_all
      (fun a -> a) (* Autres bâtiments qui gênent ? *)
      (List.init structure.width (fun i ->
           List.for_all
             (function
               | { coords = { overground; _ }; _ }
                 when overground = structure.overground ->
                   false
               | _ -> true)
             (get city.cells (x + i)).ground))
    && List.for_all
         (function
           | { coords = { overground; _ }; width = 2; _ }
             when overground = structure.overground ->
               false
           | _ -> true)
         (get city.cells (x - 1)).ground

  let is_underground_buildable structure c =
    if structure = Structures.minerCamp then
      List.for_all (fun wS -> wS.structure = Structures.seam) c.ground
    else if structure = Structures.well then
      List.length c.ground >= 1
      && List.for_all (fun wS -> wS.structure = Structures.groundwater) c.ground
    else if structure = Structures.derrick then
      List.length c.ground >= 1
      && List.for_all (fun wS -> wS.structure = Structures.oilDeposit) c.ground
    else true

  let can_build city (structure : structure) x =
    let pc = get city.cells (x - 1) in
    let c = get city.cells x in
    let nc = get city.cells (x + 1) in
    (* Partie découverte ? *)
    c.explored = Yes
    && (structure.width < 2 || nc.explored = Yes)
    && ((* Biome constructible ? *)
        Biomes.is_flat c.biome
        && structure <> Structures.crops
        && structure <> Structures.harbor
        && structure <> Structures.boat
        && structure <> Structures.tunnel
       || (c.biome = Biomes.plain && structure = Structures.crops)
       || c.biome = Biomes.ocean
          && structure = Structures.harbor
          && (pc.biome <> Biomes.ocean || nc.biome <> Biomes.ocean)
       || (c.biome = Biomes.ocean
          && structure = Structures.boat
          &&
          let cell_contains_harbor cell =
            List.exists
              (fun ws -> ws.structure == Structures.harbor)
              cell.ground
          in
          cell_contains_harbor pc || cell_contains_harbor nc)
       || (c.biome = Biomes.mountain && structure = Structures.tunnel))
    && (structure.width < 2 || Biomes.is_flat nc.biome)
    && is_cell_empty city structure x
    && is_underground_buildable structure c

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
    let view_distance cell = if Biomes.is_flat cell.biome then 3 else 1 in
    let rec update x inc distance =
      let cell = get city.cells x in
      let distance = min (view_distance cell) distance in
      if distance > 0 then (
        cell.explored <- Yes;
        update (x + inc) inc (distance - 1))
      else if distance = 0 then cell.explored <- max cell.explored Discovered
    in
    update (x - 1) (-1) (view_distance (get city.cells (x - 1)));
    update (x + 1) 1 (view_distance (get city.cells (x + 1)))

  let upgrade city wS =
    if
      wS.level < last wS.structure.levels
      && can_buy city (wS.structure, wS.level + 1)
      && wS.structure <> Structures.boat
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
        you_win city)
    else if
      wS.structure = Structures.boat
      && is_cell_empty city wS.structure (wS.coords.x - 1)
      && (get city.cells (wS.coords.x - 1)).biome = Biomes.ocean
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
      wS.level <- 0;
      explore city wS.coords.x)

  let downgrade city wS =
    if wS.level > 0 && wS.structure <> Structures.boat then (
      let res =
        List.map
          (fun r -> { r with amount = -.r.amount })
          (List.filter
             (fun r -> Resources.is_not_producible r.name)
             wS.structure.levels.(wS.level).initialResources)
      in
      city.availableResources <-
        Calculations.sum_resources [ city.availableResources; res ];
      wS.initialResources <-
        Calculations.sum_resources [ wS.initialResources; res ];
      wS.level <- wS.level - 1;
      let l = wS.structure.levels.(wS.level) in
      wS.storage <- l.storage;
      update_storage city)
    else if
      wS.structure = Structures.boat
      && is_cell_empty city wS.structure (wS.coords.x + 1)
      && (get city.cells (wS.coords.x + 1)).biome = Biomes.ocean
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
      wS.level <- 1;
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
                 (fun r -> r.amount > 0. || Resources.is_not_producible r.name)
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

  let generate_cells seed width =
    let get_biome x =
      let w = 1. /. 12. in
      let h x =
        let z =
          (foi x *. w)
          +. foi (seed * 181 mod 1103)
          +. (foi (seed mod 2753) /. 137.)
        in
        let f z =
          (3. *. sin (3. *. z))
          +. (sin z *. cos z /. 2.)
          +. (3. *. sin (4. *. z) *. cos (10. *. z))
          +. 2.
        in
        min (f z) (f (z +. w)) *. Float.abs (f z)
      in
      let y = h x in
      let seeded_x = x + seed in
      match if y < 0. then 0 else if y > 27. then 2 else 1 with
      | 0 -> Biomes.ocean
      | 2 -> Biomes.mountain
      | _ when seeded_x mod 12 >= 8 -> Biomes.desert
      | _ -> Biomes.plain
    in
    Array.mapi
      (fun x biome ->
        let ground = ref [] in
        let biome = ref biome in
        let seeded_x = x + seed in
        if x >= width - 2 || (x >= 0 && x <= 4) then
          if not (Biomes.is_flat !biome) then biome := Biomes.plain;
        if (x = 5 || x = 6) && get_biome 7 <> Biomes.ocean then
          (* On retire le pédiluve *)
          biome := Biomes.plain;
        if
          (x = width - 3 || x = width - 4)
          && get_biome (width - 5) <> Biomes.ocean
        then biome := Biomes.desert;
        let place_wood x =
          (not (x >= width - 2 || (x >= 0 && x <= 4)))
          && (seeded_x mod 10 >= 6
              && seeded_x mod 10 <= 9
              && Biomes.is_flat (get_biome (x - 1))
              && Biomes.is_flat (get_biome (x + 1))
             || seeded_x mod 10 >= 1
                && seeded_x mod 10 <= 4
                && Biomes.is_flat (get_biome (x - 1))
                && Biomes.is_flat (get_biome (x + 1)))
        in
        if x = 0 then
          ground :=
            new_worldStructure (Structures.cityHall, 0) x true 0. :: !ground
        else if x = 2 then (
          ground :=
            new_worldStructure (Structures.well, 0) x true 0.
            :: water x width :: !ground;
          biome := Biomes.plain)
        else if x = width - 2 || x = 4 then (
          ground := wood x width :: !ground;
          biome := Biomes.plain)
        else if x <> 1 then (
          if !biome = Biomes.plain && place_wood x then
            ground := wood x width :: !ground;
          if !biome = Biomes.plain && seeded_x mod 4 = 2 then
            ground := water x width :: !ground;
          if !biome = Biomes.plain && seeded_x mod 8 = 3 then
            ground := metal x width :: !ground;
          if !biome = Biomes.desert && seeded_x mod 3 = 0 then
            ground := metal x width :: !ground;
          if !biome = Biomes.desert && seeded_x mod 5 = 0 && seeded_x mod 3 <> 0
          then ground := oil x width :: !ground);
        {
          biome = !biome;
          ground = !ground;
          x;
          explored =
            (if abs ((width / 2) + 1 - x) >= (width / 2) - 3 then Yes
            else if abs ((width / 2) + 1 - x) >= (width / 2) - 4 then Discovered
            else No);
        })
      (Array.init width get_biome)

  let new_city seed =
    {
      width = Defaults.planetWidth;
      cells = generate_cells seed Defaults.planetWidth;
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
         (fun r -> r.amount > 0. || Resources.is_not_producible r.name)
         (List.hd city.interactions.selection).initialResources)

  let earned_on_downgrade city =
    let wS = List.hd city.interactions.selection in
    List.map
      (fun r -> { r with amount = -.r.amount })
      (List.filter
         (fun r -> Resources.is_not_producible r.name)
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

let new_game seed = { cities = [| City.new_city seed |] }

let update game timeSinceLastUpdate =
  for i = 0 to last game.cities do
    let city = game.cities.(i) in
    if city.state = Playing then City.update_city city timeSinceLastUpdate
  done
