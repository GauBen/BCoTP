open Aliases

type pixels = { x : int; y : int } (* Coordonnées dans la fenêtre *)

type mouse = {
  mutable position : pixels;
  mutable elementPosition : pixels;
  mutable down : bool;
}

type uiElement = {
  mutable position : pixels;
  mutable sizes : pixels;
  mutable draw : uiElement -> ui -> unit;
  mutable onMouseMove : eventHandler option;
  mutable onMouseOver : eventHandler option;
  mutable onMouseOut : eventHandler option;
  mutable onMouseDown : eventHandler option;
  mutable onMouseClick : eventHandler option;
  mutable onMouseUp : eventHandler option;
  mutable children : uiElement list;
}

and ui = {
  city : Game.city;
  screen : screen;
  mouse : mouse;
  scene : scene;
  hud : hud;
  mutable tick : int;
  (* Pour les animations *)
  mutable hoveredElement : uiElement;
  mutable clickedElement : uiElement;
}

and eventHandler = uiElement -> ui -> unit

and screen = { screen_element : uiElement }

(* Scène / Plateau de jeu *)
and scene = {
  mutable cameraX : float;
  (* Position horizontale de la caméra *)
  mutable tremor : pixels;
  (* Pour un effet de secousse *)
  mutable scene_scale : int;
  (* Taille (en pixels) d'un carré du jeu *)
  mutable selection : Game.coords option;
  scene_element : uiElement;
}

(* Head-Up Display / Données affichées pour le joueur *)
and hud = {
  mutable visible : bool;
  mutable scale : int;
  (* Affiche la production ou les ressources disponibles *)
  mutable showProduction : bool;
  element : uiElement;
  mutable infoTip : uiElement option;
}

module KeyboardControls = struct
  let is_left k = k = 97 || k = 113 || k = 52 (* A, Q et 4 *)

  let is_right k = k = 100 || k = 54 (* D et 6 *)

  (* +, =, I, Z et W *)
  let is_zoom_in k = k = 43 || k = 61 || k = 105 || k = 122 || k = 119

  let is_zoom_out k = k = 45 || k = 111 || k = 115 (* -, O et S *)

  let is_prod k = k = 112 (* P *)

  let is_esc k = k = 8 || k = 27 || k = 120 (* Backspace, Esc et X *)
end

(* / KeyboardControls *)

(* Mise en page du menu de construction *)
module Layout = struct
  let buildingPanelCols = 5

  let buildingPanelRows = 3

  let position s =
    match s with
    | i when i == Game.Structures.house -> (0, 0)
    | i when i == Game.Structures.crops -> (1, 0)
    | i when i == Game.Structures.lumberCamp -> (2, 0)
    | i when i == Game.Structures.minerCamp -> (3, 0)
    | i when i == Game.Structures.well -> (4, 0)
    | i when i == Game.Structures.building -> (0, 1)
    | i when i == Game.Structures.farm -> (1, 1)
    | i when i == Game.Structures.harbor -> (2, 1)
    | i when i == Game.Structures.boat -> (3, 1)
    | i when i == Game.Structures.tunnel -> (4, 1)
    | i when i == Game.Structures.coalFiredPlant -> (0, 2)
    | i when i == Game.Structures.derrick -> (1, 2)
    | i when i == Game.Structures.university -> (2, 2)
    | i when i == Game.Structures.windTurbine -> (3, 2)
    | i when i == Game.Structures.rocketStation -> (4, 2)
    | _ -> failwith "Layout / Structure absente du menu"
end

(* / Layout *)

module Shaders = struct
  let no _ _ c = c

  let checkerBoardNope i j c =
    if c = 0 || i mod 2 = 0 <> (j mod 2 = 0) then 0 else 28

  let checkerBoardOk i j c =
    if c = 0 || i mod 2 = 0 <> (j mod 2 = 0) then 0 else 22

  let dimmed i j c =
    if c = 0 then 0 else if i mod 2 = 0 <> (j mod 2 = 0) then c else 1

  let dark _ _ c = if c = 0 then 0 else 1

  let disabled i j c =
    if c = 0 then 0 else if i mod 2 = 0 <> (j mod 2 = 0) then c else 26
end

(* Calculs des tailles *)
module Pixels = struct
  let scene = 6

  let per_square ui = ui.scene.scene_scale

  let per_building ui = ui.scene.scene_scale * Resources.Widths.building

  let per_block ui = ui.scene.scene_scale * Resources.Widths.block

  let surface ui = ui.scene.scene_element.sizes.y / 3

  let curvature scene' _ = scene'.sizes.y / 10

  let hud_scale screen = 2 + (screen.screen_element.sizes.y / 400)

  let hud_height scale =
    (2 + ((Resources.Font.y + 2) * Game.Resources.length)) * scale

  let resource_panel_width scale =
    ((2 * 2) + Resources.Font.y + (9 * Resources.Font.x) + 9 + 4) * scale

  let building_panel_width scale =
    ((3 * 2) + ((Resources.Widths.button + 2) * Layout.buildingPanelCols) - 2)
    * scale

  let info_panel_width scale =
    ((2 * 2) + Resources.Font.y + (9 * Resources.Font.x) + 9 + 3) * scale

  let infoTip_width scale =
    ((2 * 2) + Resources.Font.y + (4 * Resources.Font.x) + 5) * scale

  let hud_margin width scale =
    (width / 2)
    - (resource_panel_width scale + info_panel_width scale
     + building_panel_width scale + 2)
      / 2
end

(* / Pixels *)

module Calculations = struct
  let gradient s c1 c2 =
    let f a b = iof (((1. -. s) *. foi a) +. (s *. foi b)) in
    let r = f (c1 / 65536) (c2 / 65536) in
    let g = f (c1 / 256 mod 256) (c2 / 256 mod 256) in
    let b = f (c1 mod 256) (c2 mod 256) in
    Graphics.rgb r g b

  let curve scene' ui x =
    let width = scene'.sizes.x in
    let drop = Pixels.curvature scene' ui in
    (-x * (x - width) * drop / ((width * width / 4) + 1)) - drop

  let most_distant_blocks scene' ui =
    let cx = ui.scene.cameraX in
    let delta = foi scene'.sizes.x /. (2. *. foi (Pixels.per_block ui)) in
    (iof (floor (cx -. delta)) - 1, iof (floor (cx +. delta)))

  let mouse_game_coords scene' ui : Game.coords =
    let mx = ui.mouse.elementPosition.x in
    let x =
      iof
        (floor
           (ui.scene.cameraX
           +. foi ((2 * mx) - scene'.sizes.x)
              /. (2. *. foi (Pixels.per_block ui))))
    in
    let curvation = curve scene' ui mx in
    {
      x;
      overground = ui.mouse.elementPosition.y >= Pixels.surface ui + curvation;
    }
end

(* / Calculations *)

module Game2Resources = struct
  let structure (s : Game.structure) =
    match s with
    | i when i = Game.Structures.cityHall -> Resources.Structures.cityHall
    | i when i = Game.Structures.house -> Resources.Structures.house
    | i when i = Game.Structures.building -> Resources.Structures.building
    | i when i = Game.Structures.well -> Resources.Structures.well
    | i when i = Game.Structures.crops -> Resources.Structures.crops
    | i when i = Game.Structures.farm -> Resources.Structures.farm
    | i when i = Game.Structures.minerCamp -> Resources.Structures.minerCamp
    | i when i = Game.Structures.lumberCamp -> Resources.Structures.lumberCamp
    | i when i = Game.Structures.harbor -> Resources.Structures.harbor
    | i when i = Game.Structures.boat -> Resources.Structures.boat
    | i when i = Game.Structures.tunnel -> Resources.Structures.tunnel
    | i when i = Game.Structures.forest -> Resources.Structures.forest
    | i when i = Game.Structures.groundwater -> Resources.Structures.groundwater
    | i when i = Game.Structures.seam -> Resources.Structures.seam
    | i when i = Game.Structures.oilDeposit -> Resources.Structures.oilDeposit
    | i when i = Game.Structures.field -> Resources.Structures.field
    | i when i = Game.Structures.derrick -> Resources.Structures.derrick
    | i when i = Game.Structures.university -> Resources.Structures.university
    | i when i = Game.Structures.rocketStation ->
        Resources.Structures.rocketStation
    | i when i = Game.Structures.coalFiredPlant ->
        Resources.Structures.coalFiredPlant
    | i when i = Game.Structures.windTurbine -> Resources.Structures.windTurbine
    | _ -> failwith "Game2Resources / Structure inconnue"

  let icon name =
    match name with
    | i when i = Game.Resources.villagers -> Resources.Icons.villagers
    | i when i = Game.Resources.water -> Resources.Icons.water
    | i when i = Game.Resources.food -> Resources.Icons.food
    | i when i = Game.Resources.wood -> Resources.Icons.wood
    | i when i = Game.Resources.sand -> Resources.Icons.sand
    | i when i = Game.Resources.metal -> Resources.Icons.metal
    | i when i = Game.Resources.oil -> Resources.Icons.oil
    | i when i = Game.Resources.electricity -> Resources.Icons.electricity
    | _ -> failwith "Game2Resources / Icone inconnue"

  let biome b1 b2 =
    match (b1, b2) with
    | i, j when i == Game.Biomes.plain && j == Game.Biomes.plain ->
        Resources.Biomes.plainPlain
    | i, j when i == Game.Biomes.plain && j == Game.Biomes.desert ->
        Resources.Biomes.plainDesert
    | i, j when i == Game.Biomes.plain && j == Game.Biomes.ocean ->
        Resources.Biomes.plainOcean
    | i, j when i == Game.Biomes.plain && j == Game.Biomes.mountain ->
        Resources.Biomes.plainMountain
    | i, j when i == Game.Biomes.desert && j == Game.Biomes.plain ->
        Resources.Biomes.desertPlain
    | i, j when i == Game.Biomes.desert && j == Game.Biomes.desert ->
        Resources.Biomes.desertDesert
    | i, j when i == Game.Biomes.desert && j == Game.Biomes.ocean ->
        Resources.Biomes.desertOcean
    | i, j when i == Game.Biomes.desert && j == Game.Biomes.mountain ->
        Resources.Biomes.desertMountain
    | i, j when i == Game.Biomes.ocean && j == Game.Biomes.plain ->
        Resources.Biomes.oceanPlain
    | i, j when i == Game.Biomes.ocean && j == Game.Biomes.desert ->
        Resources.Biomes.oceanDesert
    | i, j when i == Game.Biomes.ocean && j == Game.Biomes.ocean ->
        Resources.Biomes.oceanOcean
    | i, j when i == Game.Biomes.ocean && j == Game.Biomes.mountain ->
        Resources.Biomes.oceanMountain
    | i, j when i == Game.Biomes.mountain && j == Game.Biomes.plain ->
        Resources.Biomes.mountainPlain
    | i, j when i == Game.Biomes.mountain && j == Game.Biomes.desert ->
        Resources.Biomes.mountainDesert
    | i, j when i == Game.Biomes.mountain && j == Game.Biomes.ocean ->
        Resources.Biomes.mountainOcean
    | i, j when i == Game.Biomes.mountain && j == Game.Biomes.mountain ->
        Resources.Biomes.mountainMountain
    | _ -> failwith "Game2Resources / Biome inconnu"

  let button (s : Game.structure) =
    match s with
    | i when i = Game.Structures.house -> Resources.Buttons.house
    | i when i = Game.Structures.crops -> Resources.Buttons.crops
    | i when i = Game.Structures.well -> Resources.Buttons.well
    | i when i = Game.Structures.minerCamp -> Resources.Buttons.minerCamp
    | i when i = Game.Structures.lumberCamp -> Resources.Buttons.lumberCamp
    | i when i = Game.Structures.building -> Resources.Buttons.building
    | i when i = Game.Structures.harbor -> Resources.Buttons.harbor
    | i when i = Game.Structures.boat -> Resources.Buttons.boat
    | i when i = Game.Structures.tunnel -> Resources.Buttons.tunnel
    | i when i = Game.Structures.farm -> Resources.Buttons.farm
    | i when i = Game.Structures.derrick -> Resources.Buttons.derrick
    | i when i = Game.Structures.university -> Resources.Buttons.university
    | i when i = Game.Structures.rocketStation ->
        Resources.Buttons.rocketStation
    | i when i = Game.Structures.coalFiredPlant ->
        Resources.Buttons.coalFiredPlant
    | i when i = Game.Structures.windTurbine -> Resources.Buttons.windTurbine
    | _ -> failwith "Game2Resources / Bouton inconnu"
end

(* / Game2Resources *)

(* Fonctions de dessin *)
module Draw = struct
  let _fill_rect =
    let _fill_rect_unix { x; y } { x = width; y = height } =
      Graphics.fill_rect x y (width - 1) (height - 1)
    in
    let _fill_rect_win32 { x; y } { x = width; y = height } =
      Graphics.fill_rect x y width height
    in
    if Sys.unix then _fill_rect_unix else _fill_rect_win32

  let _fill_rect_game_fixed scene' _ { x; y } =
    _fill_rect { x = x + scene'.position.x; y = y + scene'.position.y }

  let _fill_rect_game scene' ui { x; y } =
    _fill_rect_game_fixed scene' ui
      { x = x + ui.scene.tremor.x; y = y + ui.scene.tremor.y }

  (* Dessine une Resources.bitmap avec des effets *)
  let _draw_bitmap (fill_rect, curve, shade) bitmap { x; y } scale =
    let ly, lx = (last bitmap, last bitmap.(0)) in
    for i = 0 to ly do
      for j = 0 to lx do
        let c = Resources.palette.(shade i j bitmap.(i).(j)) in
        if c <> Graphics.transp then (
          Graphics.set_color c;
          fill_rect
            {
              x = x + (j * scale);
              y = y + ((ly - i) * scale) + curve (x + (j * scale));
            }
            { x = scale; y = scale })
      done
    done

  let draw_bitmap bitmap { x; y } scale =
    _draw_bitmap (_fill_rect, (fun _ -> 0), Shaders.no) bitmap { x; y } scale

  let _write_digit ui n { x; y } =
    draw_bitmap Resources.Font.numbers.(n) { x; y } ui.hud.scale

  let _draw_character ui c { x; y } = draw_bitmap c { x; y } ui.hud.scale

  let _draw_digit ui n { x; y } =
    _draw_character ui Resources.Font.numbers.(n) { x; y }

  let draw_number ui n { x; y } (signed, rightAligned, pad) =
    let margin i = i * (Resources.Font.x + 1) * ui.hud.scale in
    let a = abs n in
    let s = string_of_int a in
    let sign = if signed && a > 0 then 1 else 0 in
    let l = String.length s + sign in
    let offset =
      if rightAligned > 0 && pad == None then rightAligned - l else 0
    in
    if sign = 1 then
      _draw_character ui
        (if n > 0 then Resources.Font.plus else Resources.Font.minus)
        { x = x + margin offset; y };
    String.iteri
      (fun i c ->
        _draw_digit ui (ord c - 48) { x = x + margin (i + offset + sign); y })
      (match pad with
      | None -> s
      | Some c -> String.make (rightAligned - l) c ^ s)

  let draw_padded_number ui n l { x; y } =
    draw_number ui n { x; y } (false, l, None)

  let draw_padded_number_zeroes ui n l { x; y } =
    draw_number ui n { x; y } (false, l, Some '0')

  let draw_padded_signed_number ui n l { x; y } =
    draw_number ui n { x; y } (true, l, None)

  let _draw_on_scene scene' ui shade bitmap { x; y } =
    _draw_bitmap
      (_fill_rect_game scene' ui, Calculations.curve scene' ui, shade)
      bitmap { x; y } ui.scene.scene_scale

  let _draw_gif scene' ui shade (gif : Resources.gif)
      ({ x; overground } : Game.coords) =
    let bitmap = gif.frames.(ui.tick mod gif.length) in
    let gc = Pixels.per_block ui in
    _draw_on_scene scene' ui shade bitmap
      {
        x =
          iof ((foi scene'.sizes.x /. 2.) -. (ui.scene.cameraX *. foi gc))
          + (x * gc);
        y =
          (Pixels.surface ui
          + (ui.scene.scene_scale * if overground then 0 else -len bitmap));
      }

  let _draw_overground scene' ui shade gif x =
    _draw_gif scene' ui shade gif { x; overground = true }

  let _draw_underground scene' ui shade gif x =
    _draw_gif scene' ui shade gif { x; overground = false }

  let _draw_shaded_block scene' ui (top, bottom) (block : Resources.block) x =
    (match block with
    | { overground = Some gif; _ } -> _draw_overground scene' ui top gif x
    | _ -> ());
    match block with
    | { underground = Some gif; _ } -> _draw_underground scene' ui bottom gif x
    | _ -> ()

  let _draw_block scene' ui =
    _draw_shaded_block scene' ui (Shaders.no, Shaders.no)

  let _draw_shaded_structure scene' ui (top, bottom)
      ((s : Game.structure), level) x =
    let structure = Game2Resources.structure s in
    let block = structure.levels.(level) in
    _draw_shaded_block scene' ui (top, bottom) block x

  let _draw_structure scene' ui ((s : Game.structure), level) x =
    _draw_shaded_structure scene' ui (Shaders.no, Shaders.no) (s, level) x

  (* Affiche le ciel et la forme de la planète *)
  let draw_sky scene' ui =
    let steps = 100 in
    let h, w = (scene'.sizes.y, scene'.sizes.x) in
    for i = 0 to steps do
      Graphics.set_color
        (Calculations.gradient
           (foi i /. foi steps)
           Resources.palette.(22) Resources.palette.(20));
      let y = i * h / steps in
      _fill_rect_game_fixed scene' ui { x = 0; y }
        { x = w; y = ((i + 1) * h / steps) - y }
    done;
    Graphics.set_color Resources.palette.(1);
    let leftMost, rightMost = Calculations.most_distant_blocks scene' ui in
    let scale = ui.scene.scene_scale in
    let gc = Pixels.per_block ui in
    let cx = ui.scene.cameraX in
    let h posx = Pixels.surface ui + Calculations.curve scene' ui posx in
    for x = 0 to (rightMost - leftMost + 1) * Resources.Widths.block do
      let posx =
        iof ((foi scene'.sizes.x /. 2.) -. (cx *. foi gc))
        + (leftMost * gc) + (x * scale)
      in
      if -scale <= posx && posx <= scene'.sizes.x then
        _fill_rect_game scene' ui
          { x = posx; y = -scene'.position.y - 1 }
          { x = scale; y = scene'.position.y + h posx }
    done

  let draw_ground scene' ui =
    let city = ui.city in
    let leftMost, rightMost = Calculations.most_distant_blocks scene' ui in
    for x = leftMost to rightMost do
      let cell = get city.cells x in
      let biome = cell.biome in
      let nextBiome = (get city.cells (x + 1)).biome in
      let s = Game2Resources.biome biome nextBiome in
      let shader =
        match cell.explored with
        | Yes -> (Shaders.no, Shaders.no)
        | Discovered -> (Shaders.dimmed, Shaders.dimmed)
        | No -> (Shaders.dark, Shaders.dark)
      in
      _draw_shaded_block scene' ui shader s x
    done;
    for x = leftMost to rightMost do
      let cell = get city.cells x in
      let shader =
        match cell.explored with
        | Yes -> (Shaders.no, Shaders.no)
        | Discovered -> (Shaders.dimmed, Shaders.dimmed)
        | No -> (Shaders.dark, Shaders.dark)
      in
      List.iter
        (fun (s : Game.worldStructure) ->
          _draw_shaded_structure scene' ui
            (if s.working then shader else (Shaders.disabled, Shaders.disabled))
            (s.structure, s.level) x)
        (List.rev cell.ground)
    done

  (* Indications de construction *)
  let draw_hints scene' ui =
    match (scene' == ui.hoveredElement, ui.city.interactions.toBeBuilt) with
    | true, Some s ->
        let ({ x; overground } : Game.coords) =
          Calculations.mouse_game_coords scene' ui
        in
        if overground = s.overground then
          let shaders =
            if get ui.city.interactions.possibleCoords x then
              (Shaders.checkerBoardOk, Shaders.checkerBoardOk)
            else (Shaders.checkerBoardNope, Shaders.checkerBoardNope)
          in
          _draw_shaded_structure scene' ui shaders (s, 0) x
    | _ -> ()

  let draw_scene scene' ui =
    draw_sky scene' ui;
    draw_ground scene' ui;
    draw_hints scene' ui

  let _draw_hud_availableRes element ui =
    let { x; y } = element.position in
    let draw thing (x2, y2) =
      draw_bitmap thing { x = x + x2; y = y + y2 } ui.hud.scale
    in
    List.iter
      (fun (aR : Game.resource) ->
        let y2 =
          (2 + ((Game.Resources.length - aR.name) * (Resources.Font.y + 2)))
          * ui.hud.scale
        in
        draw (Game2Resources.icon aR.name) (2 * ui.hud.scale, y + y2);
        draw_padded_number ui (iof aR.amount) 4
          { x = x + ((4 + Resources.Font.y) * ui.hud.scale); y = y + y2 };
        draw Resources.Font.slash
          ( (4 + (Resources.Font.x * 4) + 3 + Resources.Font.y + 2)
            * ui.hud.scale,
            y + y2 ))
      ui.city.availableResources;
    List.iter
      (fun (s : Game.resource) ->
        let y2 =
          (2 + ((Game.Resources.length - s.name) * (Resources.Font.y + 2)))
          * ui.hud.scale
        in
        draw_padded_number ui (iof s.amount) 4
          {
            x =
              x
              + (4 + (Resources.Font.x * 5) + 3 + 2 + Resources.Font.y + 2)
                * ui.hud.scale;
            y = y + y2;
          })
      ui.city.storage

  let _draw_timer draw ui x m y y2 =
    draw Resources.Icons.clock (2 * ui.hud.scale, y + y2);
    let nb n x2 =
      draw_padded_number_zeroes ui n 2
        {
          x =
            x
            + (4 + Resources.Font.y + 3 + (x2 * (Resources.Font.x + 1)) + m)
              * ui.hud.scale;
          y = y + y2;
        }
    in
    nb (iof ui.city.time / 3600 mod 100) 0;
    nb (iof ui.city.time / 60 mod 60) 3;
    nb (iof ui.city.time mod 60) 6;
    let colon x2 =
      draw Resources.Font.colon
        ( (4 + Resources.Font.y + 3 + (x2 * (Resources.Font.x + 1)) + m)
          * ui.hud.scale,
          y + y2 )
    in
    colon 2;
    colon 5

  let _draw_hud_production element ui =
    let { x; y } = element.position in
    let draw thing (x2, y2) =
      draw_bitmap thing { x = x + x2; y = y + y2 } ui.hud.scale
    in
    List.iter
      (fun (p : Game.resource) ->
        let y2 =
          (2 + ((Game.Resources.length - p.name) * (Resources.Font.y + 2)))
          * ui.hud.scale
        in
        if p.name = Game.Resources.villagers then _draw_timer draw ui x 0 y y2
        else if p.name = Game.Resources.electricity then (
          let draw thing (x2, y2) =
            draw_bitmap thing { x = x + x2; y = y + y2 } ui.hud.scale
          in
          draw (Game2Resources.icon p.name) (2 * ui.hud.scale, y + y2);
          draw_padded_number ui
            (iof
               (Game.Calculations.get_resource ui.city.availableResources
                  p.name))
            4
            { x = x + ((4 + Resources.Font.y) * ui.hud.scale); y = y + y2 };
          draw Resources.Font.slash
            ( (4 + (Resources.Font.x * 4) + 3 + Resources.Font.y + 2)
              * ui.hud.scale,
              y + y2 );
          draw_padded_number ui
            (iof (Game.Calculations.get_resource ui.city.storage p.name))
            4
            {
              x =
                x
                + (4 + (Resources.Font.x * 5) + 3 + 2 + Resources.Font.y + 2)
                  * ui.hud.scale;
              y = y + y2;
            })
        else (
          draw (Game2Resources.icon p.name) (2 * ui.hud.scale, y + y2);
          draw_padded_signed_number ui
            (iof (p.amount *. 60.))
            4
            { x = x + ((4 + Resources.Font.y) * ui.hud.scale); y = y + y2 };
          draw Resources.Font.slash
            ( (4 + (Resources.Font.x * 4) + 3 + Resources.Font.y + 2)
              * ui.hud.scale,
              y + y2 );
          draw Resources.Font.min
            ( (4 + (Resources.Font.x * 5) + 3 + 2 + Resources.Font.y + 2)
              * ui.hud.scale,
              y + y2 )))
      ui.city.production

  let draw_hud_resources element ui =
    (if ui.hud.showProduction then _draw_hud_production
    else _draw_hud_availableRes)
      element ui

  let draw_hud_buildings _ _ = ()

  (* Infos sur les bâtiments sélectionnés *)
  let draw_hud_info box' ui =
    let { x; y } = box'.position in
    if List.length ui.city.interactions.selection >= 1 then (
      let y2 i =
        box'.sizes.y + box'.position.y
        - (ui.hud.scale * (i + 1) * (Resources.Font.y + 2))
      in
      let selection = List.nth ui.city.interactions.selection 0 in
      let lvl =
        if selection.structure = Game.Structures.boat then 1
        else selection.level + 1
      in
      _draw_character ui Resources.Font.lvl
        { x = x + (17 * ui.hud.scale); y = y2 0 };
      draw_padded_number ui lvl 1 { x = x + (28 * ui.hud.scale); y = y2 0 };
      let i = ref 1 in
      List.iter
        (fun (wS : Game.worldStructure) ->
          if wS.player's then (
            List.iter
              (fun (initialResource : Game.resource) ->
                if
                  initialResource.amount > 0.
                  || initialResource.name = Game.Resources.villagers
                then (
                  draw_bitmap
                    (Game2Resources.icon initialResource.name)
                    { x = x + (2 * ui.hud.scale); y = y + y2 !i }
                    ui.hud.scale;
                  draw_padded_signed_number ui
                    (iof initialResource.amount)
                    4
                    {
                      x = x + ((4 + Resources.Font.y) * ui.hud.scale);
                      y = y + y2 !i;
                    };
                  incr i))
              wS.initialResources;
            List.iter
              (fun (p : Game.resource) ->
                draw_bitmap
                  (Game2Resources.icon p.name)
                  { x = x + (2 * ui.hud.scale); y = y + y2 !i }
                  ui.hud.scale;
                draw_padded_signed_number ui
                  (iof (p.amount *. 60.))
                  4
                  {
                    x = x + ((4 + Resources.Font.y) * ui.hud.scale);
                    y = y + y2 !i;
                  };
                _draw_character ui Resources.Font.slash
                  {
                    x =
                      x
                      + (4 + (Resources.Font.x * 4) + 3 + Resources.Font.y + 2)
                        * ui.hud.scale;
                    y = y + y2 !i;
                  };
                _draw_character ui Resources.Font.min
                  {
                    x =
                      x
                      + (4 + (Resources.Font.x * 5) + 3 + 2 + Resources.Font.y
                       + 2)
                        * ui.hud.scale;
                    y = y + y2 !i;
                  };
                incr i)
              wS.production)
          else
            List.iter
              (fun (initialResource : Game.resource) ->
                if initialResource.amount >= 0. then (
                  let storage =
                    Game.Calculations.get_resource wS.initialResources
                      initialResource.name
                  in
                  draw_bitmap
                    (Game2Resources.icon initialResource.name)
                    { x = x + (2 * ui.hud.scale); y = y + y2 !i }
                    ui.hud.scale;
                  draw_padded_number ui
                    (iof initialResource.amount)
                    4
                    {
                      x = x + ((4 + Resources.Font.y) * ui.hud.scale);
                      y = y + y2 !i;
                    };
                  _draw_character ui Resources.Font.slash
                    {
                      x =
                        x
                        + (4 + (Resources.Font.x * 4) + 3 + Resources.Font.y + 2)
                          * ui.hud.scale;
                      y = y + y2 !i;
                    };
                  draw_padded_number ui (iof storage) 4
                    {
                      x =
                        x
                        + (4 + (Resources.Font.x * 5) + 3 + Resources.Font.y + 4)
                          * ui.hud.scale;
                      y = y + y2 !i;
                    };
                  incr i))
              wS.availableResources)
        ui.city.interactions.selection)

  let draw_hud box ui =
    draw_bitmap Resources.hud
      { x = box.position.x - (3 * ui.hud.scale); y = box.position.y }
      ui.hud.scale

  let draw_replay_button button' ui =
    draw_bitmap
      (if button' == ui.hoveredElement then
       Resources.Buttons.large_popup_button_hovered
      else Resources.Buttons.large_popup_button)
      button'.position ui.hud.scale;
    draw_bitmap Resources.Font.replay button'.position ui.hud.scale

  let draw_new_button button' ui =
    draw_bitmap
      (if button' == ui.hoveredElement then
       Resources.Buttons.small_popup_button_hovered
      else Resources.Buttons.small_popup_button)
      button'.position ui.hud.scale;
    draw_bitmap Resources.Font.text_new button'.position ui.hud.scale

  let draw_quit_button button' ui =
    draw_bitmap
      (if button' == ui.hoveredElement then
       Resources.Buttons.small_popup_button_hovered
      else Resources.Buttons.small_popup_button)
      button'.position ui.hud.scale;
    draw_bitmap Resources.Font.quit button'.position ui.hud.scale

  let draw_defeat_popup box' ui =
    draw_bitmap Resources.defeat_popup box'.position ui.hud.scale;
    let resource_corner =
      {
        x = box'.position.x + (36 * ui.hud.scale);
        y = box'.position.y + (21 * ui.hud.scale);
      }
    in
    if
      Game.Calculations.get_resource ui.city.availableResources
        Game.Resources.food
      <= 0.
    then draw_bitmap Resources.Icons.food resource_corner ui.hud.scale
    else draw_bitmap Resources.Icons.water resource_corner ui.hud.scale

  let draw_victory_popup box' ui =
    draw_bitmap Resources.victory_popup box'.position ui.hud.scale;
    _draw_timer
      (fun thing (x2, y2) ->
        draw_bitmap thing
          { x = box'.position.x + x2 + (19 * ui.hud.scale); y = y2 }
          ui.hud.scale)
      ui
      (box'.position.x + (19 * ui.hud.scale))
      (-2)
      (box'.position.y + (21 * ui.hud.scale))
      0

  (* Dessin des boutons *)
  let draw_delete_button button' ui =
    if Game.Interactions.show_delete_button ui.city then (
      draw_bitmap
        (match
           (button' == ui.hoveredElement, Game.Interactions.can_delete ui.city)
         with
        | _, false -> Resources.SmallButtons.backgroundDisabled
        | true, _ -> Resources.SmallButtons.backgroundActive
        | _ -> Resources.SmallButtons.background)
        button'.position ui.hud.scale;
      draw_bitmap Resources.SmallButtons.delete button'.position ui.hud.scale)

  let draw_up_button button' ui =
    if Game.Interactions.show_up_button ui.city then (
      draw_bitmap
        (match
           (button' == ui.hoveredElement, Game.Interactions.can_up ui.city)
         with
        | _, false -> Resources.SmallButtons.backgroundDisabled
        | true, _ -> Resources.SmallButtons.backgroundActive
        | _ -> Resources.SmallButtons.background)
        button'.position ui.hud.scale;
      draw_bitmap
        (if Game.Interactions.show_move_button ui.city then
         Resources.SmallButtons.left
        else Resources.SmallButtons.up)
        button'.position ui.hud.scale)

  let draw_down_button button' ui =
    if Game.Interactions.show_down_button ui.city then (
      draw_bitmap
        (match
           (button' == ui.hoveredElement, Game.Interactions.can_down ui.city)
         with
        | _, false -> Resources.SmallButtons.backgroundDisabled
        | true, _ -> Resources.SmallButtons.backgroundActive
        | _ -> Resources.SmallButtons.background)
        button'.position ui.hud.scale;
      draw_bitmap
        (if Game.Interactions.show_move_button ui.city then
         Resources.SmallButtons.right
        else Resources.SmallButtons.down)
        button'.position ui.hud.scale)

  let draw_building_button s button' ui =
    draw_bitmap
      (match
         ( ui.city.interactions.toBeBuilt,
           button' == ui.hoveredElement,
           Game.Interactions.can_buy ui.city s )
       with
      | _, _, false -> Resources.Buttons.backgroundDisabled
      | Some a, true, _ when a = s -> Resources.Buttons.backgroundHovered
      | Some a, _, _ when a = s -> Resources.Buttons.backgroundFocused
      | _, true, _ -> Resources.Buttons.backgroundActive
      | _ -> Resources.Buttons.background)
      button'.position ui.hud.scale;
    draw_bitmap (Game2Resources.button s) button'.position ui.hud.scale

  let draw_infoTip (p : Game.resources) tip' ui =
    let { x; y } = tip'.position in
    Graphics.set_color Resources.palette.(21);
    _fill_rect { x; y } tip'.sizes;
    let l = List.length p - 1 in
    List.iteri
      (fun i (p : Game.resource) ->
        let y2 =
          y + ((2 + ((Resources.Font.y + 2) * (l - i))) * ui.hud.scale)
        in
        draw_bitmap
          (Game2Resources.icon p.name)
          { x = x + (2 * ui.hud.scale); y = y2 }
          ui.hud.scale;
        draw_padded_signed_number ui (iof p.amount) 4
          { x = x + ((4 + Resources.Font.y) * ui.hud.scale); y = y2 })
      p

  let start_time = time ()

  let draw_splash e ui =
    let frame = iof (time () -. start_time) mod Resources.splash.length in
    let scale = ui.hud.scale * 3 in
    let py = (e.sizes.y / 2) - (len Resources.splash.frames.(0) * scale / 2) in
    let px =
      (e.sizes.x / 2) - (len Resources.splash.frames.(0).(0) * scale / 2)
    in
    draw_bitmap Resources.splash.frames.(frame) { x = px; y = py } scale

  let draw_screen _ _ = ()

  let draw_element ui element =
    let rec explore_element element =
      explore_children element.children @ [ element ]
    and explore_children children =
      match children with
      | [] -> []
      | child :: q -> explore_element child @ explore_children q
    in
    List.iter (fun e -> e.draw e ui) (List.rev (explore_element element))
end

(* / Draw *)

(* Gestion des clics *)
module EventHandlers = struct
  let play _ ui =
    ui.screen.screen_element.children <-
      [ ui.hud.element; ui.scene.scene_element ];
    ui.scene.cameraX <- 1.5;
    ui.city.state <- Playing

  let scene_click element ui =
    let coords = Calculations.mouse_game_coords element ui in
    Game.Interactions.click ui.city coords;
    ()

  let hud_resources_click _ ui =
    ui.hud.showProduction <- not ui.hud.showProduction

  let building_button_click s _ ui = Game.Interactions.want_to_build ui.city s

  let building_button_over _ infoTip' _ ui =
    ui.hud.infoTip <- Some infoTip';
    ui.hud.element.children <- infoTip' :: ui.hud.element.children

  let building_button_move _ _ ui =
    match ui.hud.infoTip with
    | Some e ->
        e.position <-
          { x = ui.mouse.position.x + 10; y = ui.mouse.position.y + 10 }
    | _ -> ()

  let building_button_out _ _ ui =
    match ui.hud.infoTip with
    | Some e ->
        ui.hud.element.children <-
          List.filter (fun a -> not (a == e)) ui.hud.element.children;
        ui.hud.infoTip <- None
    | _ -> ()

  let delete_button_over _ ui =
    if Game.Interactions.show_delete_button ui.city then
      let p : Game.resources = Game.Interactions.earned_on_destroyed ui.city in
      if List.length p >= 1 then (
        let infoTip' =
          {
            position = { x = 0; y = 0 };
            sizes =
              {
                x = Pixels.infoTip_width ui.hud.scale;
                y =
                  (2 + ((Resources.Font.y + 2) * List.length p)) * ui.hud.scale;
              };
            draw = Draw.draw_infoTip p;
            onMouseMove = None;
            onMouseOver = None;
            onMouseOut = None;
            onMouseDown = None;
            onMouseUp = None;
            onMouseClick = None;
            children = [];
          }
        in
        ui.hud.infoTip <- Some infoTip';
        ui.hud.element.children <- infoTip' :: ui.hud.element.children)

  let delete_button_move _ ui =
    match ui.hud.infoTip with
    | Some e ->
        e.position <-
          { x = ui.mouse.position.x + 10; y = ui.mouse.position.y + 10 }
    | _ -> ()

  let delete_button_out _ ui =
    match ui.hud.infoTip with
    | Some e ->
        ui.hud.element.children <-
          List.filter (fun a -> not (a == e)) ui.hud.element.children;
        ui.hud.infoTip <- None
    | _ -> ()

  let delete_button_click _ ui = Game.Interactions.destroy ui.city

  let up_button_over _ ui =
    if
      Game.Interactions.show_up_button ui.city
      && not (Game.Interactions.show_move_button ui.city)
    then
      let p : Game.resources = Game.Interactions.up_price ui.city in
      if List.length p >= 1 then (
        let infoTip' =
          {
            position = { x = 0; y = 0 };
            sizes =
              {
                x = Pixels.infoTip_width ui.hud.scale;
                y =
                  (2 + ((Resources.Font.y + 2) * List.length p)) * ui.hud.scale;
              };
            draw = Draw.draw_infoTip p;
            onMouseMove = None;
            onMouseOver = None;
            onMouseOut = None;
            onMouseDown = None;
            onMouseUp = None;
            onMouseClick = None;
            children = [];
          }
        in
        ui.hud.infoTip <- Some infoTip';
        ui.hud.element.children <- infoTip' :: ui.hud.element.children)

  let down_button_over _ ui =
    if
      Game.Interactions.show_down_button ui.city
      && not (Game.Interactions.show_move_button ui.city)
    then
      let p : Game.resources = Game.Interactions.earned_on_downgrade ui.city in
      if List.length p >= 1 then (
        let infoTip' =
          {
            position = { x = 0; y = 0 };
            sizes =
              {
                x = Pixels.infoTip_width ui.hud.scale;
                y =
                  (2 + ((Resources.Font.y + 2) * List.length p)) * ui.hud.scale;
              };
            draw = Draw.draw_infoTip p;
            onMouseMove = None;
            onMouseOver = None;
            onMouseOut = None;
            onMouseDown = None;
            onMouseUp = None;
            onMouseClick = None;
            children = [];
          }
        in
        ui.hud.infoTip <- Some infoTip';
        ui.hud.element.children <- infoTip' :: ui.hud.element.children)

  let up_button_move button' ui = delete_button_move button' ui

  let up_button_out button' ui = delete_button_out button' ui

  let up_button_click _ ui = Game.Interactions.up ui.city

  let down_button_move button' ui = delete_button_move button' ui

  let down_button_out button' ui = delete_button_out button' ui

  let down_button_click _ ui = Game.Interactions.down ui.city

  let replay_button_click _ _ = raise Replay

  let new_button_click _ _ = raise New

  let quit_button_click _ _ = raise Exit
end

(* / EventHandlers *)

(* Construction des éléments interactifs *)
module Elements = struct
  let new_scene screen (hud : hud) =
    {
      position = { x = 0; y = hud.element.sizes.y };
      sizes =
        {
          x = screen.screen_element.sizes.x;
          y = screen.screen_element.sizes.y - hud.element.sizes.y;
        };
      draw = Draw.draw_scene;
      onMouseMove = None;
      onMouseOver = None;
      onMouseOut = None;
      onMouseDown = None;
      onMouseUp = None;
      onMouseClick = Some EventHandlers.scene_click;
      children = [];
    }

  let new_hud_resources (hud : hud) =
    {
      position = { x = hud.element.position.x; y = 0 };
      sizes =
        { x = Pixels.resource_panel_width hud.scale; y = hud.element.sizes.y };
      draw = Draw.draw_hud_resources;
      onMouseMove = None;
      onMouseOver = None;
      onMouseOut = None;
      onMouseDown = None;
      onMouseUp = None;
      onMouseClick = Some EventHandlers.hud_resources_click;
      children = [];
    }

  let new_infoTip (p : Game.resources) (hud : hud) =
    {
      position = { x = 0; y = 0 };
      sizes =
        {
          x = Pixels.infoTip_width hud.scale;
          y = (2 + ((Resources.Font.y + 2) * List.length p)) * hud.scale;
        };
      draw = Draw.draw_infoTip p;
      onMouseMove = None;
      onMouseOver = None;
      onMouseOut = None;
      onMouseDown = None;
      onMouseUp = None;
      onMouseClick = None;
      children = [];
    }

  let new_hud_buildings_button s (hud : hud) hudBuildings =
    let x, y = Layout.position s in
    {
      position =
        {
          x =
            hudBuildings.position.x
            + ((3 + ((Resources.Widths.button + 2) * x)) * hud.scale);
          y =
            hudBuildings.position.y
            + (3
              + (Resources.Widths.button + 2)
                * (Layout.buildingPanelRows - y - 1))
              * hud.scale;
        };
      sizes =
        {
          x = Resources.Widths.button * hud.scale;
          y = Resources.Widths.button * hud.scale;
        };
      draw = Draw.draw_building_button s;
      onMouseMove = Some (EventHandlers.building_button_move s);
      onMouseOver =
        Some
          (EventHandlers.building_button_over s
             (new_infoTip s.levels.(0).initialResources hud));
      onMouseOut = Some (EventHandlers.building_button_out s);
      onMouseDown = None;
      onMouseUp = None;
      onMouseClick = Some (EventHandlers.building_button_click s);
      children = [];
    }

  let new_hud_buildings_buttons hud hudBuildings =
    List.map
      (fun s -> new_hud_buildings_button s hud hudBuildings)
      Game.Interactions.buildingList

  let new_hud_buildings (hud : hud) =
    let hudBuildings =
      {
        position =
          {
            x =
              hud.element.position.x + hud.scale
              + Pixels.resource_panel_width hud.scale;
            y = 0;
          };
        sizes =
          { x = Pixels.building_panel_width hud.scale; y = hud.element.sizes.y };
        draw = Draw.draw_hud_buildings;
        onMouseMove = None;
        onMouseOver = None;
        onMouseOut = None;
        onMouseDown = None;
        onMouseUp = None;
        onMouseClick = None;
        children = [];
      }
    in
    hudBuildings.children <- new_hud_buildings_buttons hud hudBuildings;
    hudBuildings

  let new_delete_button (hud : hud) info =
    {
      position =
        {
          x = info.position.x + (3 * hud.scale);
          y = info.position.y + (2 * hud.scale);
        };
      sizes =
        {
          x = Resources.Widths.smallButton * hud.scale;
          y = Resources.Widths.smallButton * hud.scale;
        };
      draw = Draw.draw_delete_button;
      onMouseMove = Some EventHandlers.delete_button_move;
      onMouseOver = Some EventHandlers.delete_button_over;
      onMouseOut = Some EventHandlers.delete_button_out;
      onMouseDown = None;
      onMouseUp = None;
      onMouseClick = Some EventHandlers.delete_button_click;
      children = [];
    }

  let new_up_button (hud : hud) info =
    {
      position =
        {
          x =
            info.position.x
            + (Resources.Widths.smallButton * hud.scale)
            + (6 * hud.scale);
          y = info.position.y + (2 * hud.scale);
        };
      sizes =
        {
          x = Resources.Widths.smallButton * hud.scale;
          y = Resources.Widths.smallButton * hud.scale;
        };
      draw = Draw.draw_up_button;
      onMouseMove = Some EventHandlers.up_button_move;
      onMouseOver = Some EventHandlers.up_button_over;
      onMouseOut = Some EventHandlers.up_button_out;
      onMouseDown = None;
      onMouseUp = None;
      onMouseClick = Some EventHandlers.up_button_click;
      children = [];
    }

  let new_down_button (hud : hud) info =
    {
      position =
        {
          x =
            info.position.x
            + (2 * Resources.Widths.smallButton * hud.scale)
            + (9 * hud.scale);
          y = info.position.y + (2 * hud.scale);
        };
      sizes =
        {
          x = Resources.Widths.smallButton * hud.scale;
          y = Resources.Widths.smallButton * hud.scale;
        };
      draw = Draw.draw_down_button;
      onMouseMove = Some EventHandlers.down_button_move;
      onMouseOver = Some EventHandlers.down_button_over;
      onMouseOut = Some EventHandlers.down_button_out;
      onMouseDown = None;
      onMouseUp = None;
      onMouseClick = Some EventHandlers.down_button_click;
      children = [];
    }

  let new_hud_info (hud : hud) =
    let info =
      {
        position =
          {
            x =
              hud.element.position.x + (3 * hud.scale)
              + Pixels.resource_panel_width hud.scale
              + Pixels.building_panel_width hud.scale;
            y = 0;
          };
        sizes =
          { x = Pixels.info_panel_width hud.scale; y = hud.element.sizes.y };
        draw = Draw.draw_hud_info;
        onMouseMove = None;
        onMouseOver = None;
        onMouseOut = None;
        onMouseDown = None;
        onMouseUp = None;
        onMouseClick = None;
        children = [];
      }
    in
    info.children <-
      [
        new_delete_button hud info; new_up_button hud info;
        new_down_button hud info;
      ];
    info

  let new_replay_button hud parent_position =
    {
      sizes =
        {
          x = len Resources.Buttons.large_popup_button.(0) * hud.scale;
          y = len Resources.Buttons.large_popup_button * hud.scale;
        };
      position =
        {
          x = parent_position.x + (5 * hud.scale);
          y = parent_position.y + (5 * hud.scale);
        };
      draw = Draw.draw_replay_button;
      onMouseMove = None;
      onMouseOver = None;
      onMouseOut = None;
      onMouseDown = None;
      onMouseUp = None;
      onMouseClick = Some EventHandlers.replay_button_click;
      children = [];
    }

  let new_new_button hud parent_position =
    {
      sizes =
        {
          x = len Resources.Buttons.small_popup_button.(0) * hud.scale;
          y = len Resources.Buttons.small_popup_button * hud.scale;
        };
      position =
        {
          x =
            parent_position.x
            + (len Resources.Buttons.large_popup_button.(0) + 2 + 5)
              * hud.scale;
          y = parent_position.y + (5 * hud.scale);
        };
      draw = Draw.draw_new_button;
      onMouseMove = None;
      onMouseOver = None;
      onMouseOut = None;
      onMouseDown = None;
      onMouseUp = None;
      onMouseClick = Some EventHandlers.new_button_click;
      children = [];
    }

  let new_quit_button hud parent_position =
    {
      sizes =
        {
          x = len Resources.Buttons.small_popup_button.(0) * hud.scale;
          y = len Resources.Buttons.small_popup_button * hud.scale;
        };
      position =
        {
          x =
            parent_position.x
            + (len Resources.Buttons.large_popup_button.(0)
              + (2 * 2) + 5
              + len Resources.Buttons.small_popup_button.(0))
              * hud.scale;
          y = parent_position.y + (5 * hud.scale);
        };
      draw = Draw.draw_quit_button;
      onMouseMove = None;
      onMouseOver = None;
      onMouseOut = None;
      onMouseDown = None;
      onMouseUp = None;
      onMouseClick = Some EventHandlers.quit_button_click;
      children = [];
    }

  let new_defeat_popup hud =
    let sizes =
      {
        x = hud.scale * len Resources.defeat_popup.(0);
        y = hud.scale * len Resources.defeat_popup;
      }
    in
    let position =
      { x = (sx () / 2) - (sizes.x / 2); y = (sy () / 2) - (sizes.y / 2) }
    in
    {
      sizes;
      position;
      draw = Draw.draw_defeat_popup;
      onMouseMove = None;
      onMouseOver = None;
      onMouseOut = None;
      onMouseDown = None;
      onMouseUp = None;
      onMouseClick = None;
      children =
        [
          new_replay_button hud position; new_new_button hud position;
          new_quit_button hud position;
        ];
    }

  let new_victory_popup hud =
    let sizes =
      {
        x = hud.scale * len Resources.victory_popup.(0);
        y = hud.scale * len Resources.victory_popup;
      }
    in
    let position =
      { x = (sx () / 2) - (sizes.x / 2); y = (sy () / 2) - (sizes.y / 2) }
    in
    {
      sizes;
      position;
      draw = Draw.draw_victory_popup;
      onMouseMove = None;
      onMouseOver = None;
      onMouseOut = None;
      onMouseDown = None;
      onMouseUp = None;
      onMouseClick = None;
      children =
        [
          new_replay_button hud position; new_new_button hud position;
          new_quit_button hud position;
        ];
    }

  let new_hud screen =
    let scale = Pixels.hud_scale screen in
    let hudHeight = Pixels.hud_height scale in
    let hud =
      {
        visible = true;
        scale;
        showProduction = false;
        element =
          {
            position =
              {
                x = Pixels.hud_margin screen.screen_element.sizes.x scale;
                y = 0;
              };
            sizes = { x = 0; y = hudHeight };
            draw = Draw.draw_hud;
            onMouseMove = None;
            onMouseOver = None;
            onMouseOut = None;
            onMouseDown = None;
            onMouseUp = None;
            onMouseClick = None;
            children = [];
          };
        infoTip = None;
      }
    in
    hud.element.children <-
      [ new_hud_resources hud; new_hud_info hud; new_hud_buildings hud ];
    hud

  let new_screen () =
    {
      screen_element =
        {
          position = { x = 0; y = 0 };
          sizes = { x = sx (); y = sy () };
          draw = Draw.draw_screen;
          onMouseMove = None;
          onMouseOver = None;
          onMouseOut = None;
          onMouseDown = None;
          onMouseUp = None;
          onMouseClick = None;
          children = [];
        };
    }

  let new_splash screen =
    {
      position = screen.screen_element.position;
      sizes = screen.screen_element.sizes;
      draw = Draw.draw_splash;
      onMouseMove = None;
      onMouseOver = None;
      onMouseOut = None;
      onMouseDown = None;
      onMouseUp = None;
      onMouseClick = Some EventHandlers.play;
      children = [];
    }
end

(* / Elements *)

(* Gestion formelle des évènements *)
module Events = struct
  let get_hovered_element ui =
    let { x = mx; y = my } = ui.mouse.position in
    let is_focused el =
      match el with
      | { position = { x; y }; sizes = { x = width; y = height }; _ } ->
          x <= mx && mx < x + width && y <= my && my < y + height
    in
    let rec explore_element element =
      explore_children element.children @ [ element ]
    and explore_children children =
      match children with
      | [] -> []
      | child :: q -> explore_element child @ explore_children q
    in
    List.find is_focused (explore_element ui.screen.screen_element)

  let trigger_mouseMove element ui =
    match element with
    | { onMouseMove = Some handler; _ } -> handler element ui
    | _ -> ()

  let trigger_mouseOver element ui =
    match element with
    | { onMouseOver = Some handler; _ } -> handler element ui
    | _ -> ()

  let trigger_mouseOut element ui =
    match element with
    | { onMouseOut = Some handler; _ } -> handler element ui
    | _ -> ()

  let trigger_mouseDown element ui =
    match element with
    | { onMouseDown = Some handler; _ } -> handler element ui
    | _ -> ()

  let trigger_mouseUp element ui =
    match element with
    | { onMouseUp = Some handler; _ } -> handler element ui
    | _ -> ()

  let trigger_mouseClick element ui =
    match element with
    | { onMouseClick = Some handler; _ } -> handler element ui
    | _ -> ()

  (* Survol ? Clic ? ... *)
  let mouse ui =
    let x, y = Graphics.mouse_pos () in
    ui.mouse.position <- { x; y };
    let element =
      try get_hovered_element ui with Not_found -> ui.screen.screen_element
    in
    ui.mouse.elementPosition <-
      {
        x = ui.mouse.position.x - element.position.x;
        y = ui.mouse.position.y - element.position.y;
      };
    match
      ( ui.hoveredElement == element,
        ui.clickedElement == element,
        ui.mouse.down,
        Graphics.button_down () )
    with
    | false, _, false, false
    | true, false, true, true
    | false, false, true, true ->
        trigger_mouseOut ui.hoveredElement ui;
        ui.hoveredElement <- element;
        trigger_mouseOver element ui;
        trigger_mouseMove element ui
    | true, _, false, false -> trigger_mouseMove element ui
    | true, _, false, true ->
        ui.mouse.down <- true;
        ui.clickedElement <- element;
        trigger_mouseDown element ui
    | true, true, true, true -> ( (* draggin' somethin' *) )
    | true, true, true, false ->
        ui.mouse.down <- false;
        trigger_mouseUp element ui;
        trigger_mouseClick element ui
    | _, false, true, false -> ui.mouse.down <- false
    | false, _, _, _ ->
        trigger_mouseOut ui.hoveredElement ui;
        ui.hoveredElement <- element
end

(* / Events *)

let new_ui city =
  let screen = Elements.new_screen () in
  let hud = Elements.new_hud screen in
  let scene =
    {
      cameraX = 1.5;
      scene_scale = Pixels.scene;
      tremor = { x = 0; y = 0 };
      selection = None;
      scene_element = Elements.new_scene screen hud;
    }
  in
  let ui =
    {
      city;
      scene;
      hud;
      mouse =
        {
          position = { x = 1; y = 0 };
          elementPosition = { x = 1; y = 0 };
          down = false;
        };
      tick = 0;
      screen;
      clickedElement = screen.screen_element;
      hoveredElement = screen.screen_element;
    }
  in
  ui.screen.screen_element.children <-
    [ ui.hud.element; ui.scene.scene_element ];
  if city.state = Paused then
    ui.screen.screen_element.children <- [ Elements.new_splash screen ];
  ref ui

let update refUi =
  let ui = !refUi in
  (if
   ui.screen.screen_element.sizes.x <> sx ()
   || ui.screen.screen_element.sizes.y <> sy ()
  then
   let ui = !(new_ui ui.city) in
   refUi := ui);
  Events.mouse ui;
  if ui.mouse.position.x = 0 then ui.scene.cameraX <- ui.scene.cameraX -. 0.2;
  if ui.mouse.position.x = ui.screen.screen_element.sizes.x - 1 then
    ui.scene.cameraX <- ui.scene.cameraX +. 0.2;
  while Graphics.key_pressed () do
    let k = ord (Graphics.read_key ()) in
    if KeyboardControls.is_left k then
      ui.scene.cameraX <- ui.scene.cameraX -. 0.1;
    if KeyboardControls.is_right k then
      ui.scene.cameraX <- ui.scene.cameraX +. 0.1;
    if KeyboardControls.is_zoom_in k then
      ui.scene.scene_scale <- ui.scene.scene_scale + 1;
    if KeyboardControls.is_zoom_out k then
      ui.scene.scene_scale <- ui.scene.scene_scale - 1;
    if KeyboardControls.is_prod k then
      ui.hud.showProduction <- not ui.hud.showProduction;
    if KeyboardControls.is_esc k then Game.Interactions.nothing ui.city
  done;
  if ui.city.state = GameOver && List.length ui.hud.element.children > 1 then
    ui.hud.element.children <- [ Elements.new_defeat_popup ui.hud ]
  else if ui.city.state = Won && List.length ui.hud.element.children > 1 then
    ui.hud.element.children <- [ Elements.new_victory_popup ui.hud ];
  ui.scene.scene_scale <- max 1 (min ui.scene.scene_scale 12);
  ui.scene.cameraX <- modfl ui.scene.cameraX ui.city.width;
  ui.tick <- iof ui.city.time

let draw refUi =
  let ui = !refUi in
  if
    ui.screen.screen_element.sizes.x <= 790
    || ui.screen.screen_element.sizes.y <= 550
  then (
    Graphics.moveto 10 10;
    Graphics.set_color Resources.palette.(1);
    Graphics.draw_string "Fenetre trop petite / Ouvrir en 800x600")
  else Draw.draw_element ui ui.screen.screen_element
