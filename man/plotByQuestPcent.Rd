\name{plotByQuestPcent}
\alias{plotByQuestPcent}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("plotByQuestPcent")}
\format{
  The format is:
List of 9
 $ data       : tibble [30 × 9] (S3: tbl_df/tbl/data.frame)
  ..$ Question: chr [1:30] "C1" "C10" "C11" "C12" ...
  ..$ Moy     : num [1:30] 2.92 1.92 2 2 2 3.94 2.08 0.85 1.94 1.92 ...
  ..$ ET      : num [1:30] 0.28 0.28 0 0 0 0.15 0.86 0.38 0.66 0.7 ...
  ..$ Min     : num [1:30] 2 1 2 2 2 3.5 1 0 1 1 ...
  ..$ Max     : num [1:30] 3 2 2 2 2 4 3 1 3 3 ...
  ..$ QNum    : num [1:30] 1 10 11 12 13 14 15 16 2 3 ...
  ..$ MaxPts  : num [1:30] 3 2 2 2 2 4 3 1 3 3 ...
  ..$ Pcent   : num [1:30] 97.3 96 100 100 100 ...
  ..$ QType   : chr [1:30] "Short" "Short" "Short" "Short" ...
 $ layers     :List of 2
  ..$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>
    aes_params: list
    compute_aesthetics: function
    compute_geom_1: function
    compute_geom_2: function
    compute_position: function
    compute_statistic: function
    computed_geom_params: list
    computed_mapping: uneval
    computed_stat_params: list
    data: waiver
    draw_geom: function
    finish_statistics: function
    geom: <ggproto object: Class GeomBar, GeomRect, Geom, gg>
        aesthetics: function
        default_aes: uneval
        draw_group: function
        draw_key: function
        draw_layer: function
        draw_panel: function
        extra_params: na.rm orientation
        handle_na: function
        non_missing_aes: xmin xmax ymin ymax
        optional_aes: 
        parameters: function
        required_aes: x y
        setup_data: function
        setup_params: function
        use_defaults: function
        super:  <ggproto object: Class GeomRect, Geom, gg>
    geom_params: list
    inherit.aes: TRUE
    layer_data: function
    map_statistic: function
    mapping: uneval
    position: <ggproto object: Class PositionStack, Position, gg>
        compute_layer: function
        compute_panel: function
        fill: FALSE
        required_aes: 
        reverse: FALSE
        setup_data: function
        setup_params: function
        type: NULL
        vjust: 1
        super:  <ggproto object: Class Position, gg>
    print: function
    setup_layer: function
    show.legend: NA
    stat: <ggproto object: Class StatIdentity, Stat, gg>
        aesthetics: function
        compute_group: function
        compute_layer: function
        compute_panel: function
        default_aes: uneval
        extra_params: na.rm
        finish_layer: function
        non_missing_aes: 
        optional_aes: 
        parameters: function
        required_aes: 
        retransform: TRUE
        setup_data: function
        setup_params: function
        super:  <ggproto object: Class Stat, gg>
    stat_params: list
    super:  <ggproto object: Class Layer, gg> 
  ..$ :Classes 'LayerInstance', 'Layer', 'ggproto', 'gg' <ggproto object: Class LayerInstance, Layer, gg>
    aes_params: list
    compute_aesthetics: function
    compute_geom_1: function
    compute_geom_2: function
    compute_position: function
    compute_statistic: function
    computed_geom_params: list
    computed_mapping: uneval
    computed_stat_params: list
    data: data.frame
    draw_geom: function
    finish_statistics: function
    geom: <ggproto object: Class GeomHline, Geom, gg>
        aesthetics: function
        default_aes: uneval
        draw_group: function
        draw_key: function
        draw_layer: function
        draw_panel: function
        extra_params: na.rm
        handle_na: function
        non_missing_aes: 
        optional_aes: 
        parameters: function
        required_aes: yintercept
        setup_data: function
        setup_params: function
        use_defaults: function
        super:  <ggproto object: Class Geom, gg>
    geom_params: list
    inherit.aes: FALSE
    layer_data: function
    map_statistic: function
    mapping: uneval
    position: <ggproto object: Class PositionIdentity, Position, gg>
        compute_layer: function
        compute_panel: function
        required_aes: 
        setup_data: function
        setup_params: function
        super:  <ggproto object: Class Position, gg>
    print: function
    setup_layer: function
    show.legend: FALSE
    stat: <ggproto object: Class StatIdentity, Stat, gg>
        aesthetics: function
        compute_group: function
        compute_layer: function
        compute_panel: function
        default_aes: uneval
        extra_params: na.rm
        finish_layer: function
        non_missing_aes: 
        optional_aes: 
        parameters: function
        required_aes: 
        retransform: TRUE
        setup_data: function
        setup_params: function
        super:  <ggproto object: Class Stat, gg>
    stat_params: list
    super:  <ggproto object: Class Layer, gg> 
 $ scales     :Classes 'ScalesList', 'ggproto', 'gg' <ggproto object: Class ScalesList, gg>
    add: function
    clone: function
    find: function
    get_scales: function
    has_scale: function
    input: function
    n: function
    non_position_scales: function
    scales: list
    super:  <ggproto object: Class ScalesList, gg> 
 $ mapping    :List of 2
  ..$ x: language ~QNum
  .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  ..$ y: language ~Pcent
  .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  ..- attr(*, "class")= chr "uneval"
 $ theme      :List of 1
  ..$ legend.position: chr "none"
  ..- attr(*, "complete")= logi FALSE
  ..- attr(*, "validate")= logi TRUE
 $ coordinates:Classes 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto object: Class CoordCartesian, Coord, gg>
    aspect: function
    backtransform_range: function
    clip: on
    default: TRUE
    distance: function
    expand: TRUE
    is_free: function
    is_linear: function
    labels: function
    limits: list
    modify_scales: function
    range: function
    render_axis_h: function
    render_axis_v: function
    render_bg: function
    render_fg: function
    setup_data: function
    setup_layout: function
    setup_panel_guides: function
    setup_panel_params: function
    setup_params: function
    train_panel_guides: function
    transform: function
    super:  <ggproto object: Class CoordCartesian, Coord, gg> 
 $ facet      :Classes 'FacetGrid', 'Facet', 'ggproto', 'gg' <ggproto object: Class FacetGrid, Facet, gg>
    compute_layout: function
    draw_back: function
    draw_front: function
    draw_labels: function
    draw_panels: function
    finish_data: function
    init_scales: function
    map_data: function
    params: list
    setup_data: function
    setup_params: function
    shrink: TRUE
    train_scales: function
    vars: function
    super:  <ggproto object: Class FacetGrid, Facet, gg> 
 $ plot_env   :<environment: R_GlobalEnv> 
 $ labels     :List of 5
  ..$ title     : chr "Pourentages réussite par question"
  ..$ y         : chr "Pourcentage réussite"
  ..$ x         : chr "QNum"
  ..$ fill      : chr "QType"
  ..$ yintercept: chr "yintercept"
 - attr(*, "class")= chr [1:2] "gg" "ggplot"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(plotByQuestPcent)
## maybe str(plotByQuestPcent) ; plot(plotByQuestPcent) ...
}
\keyword{datasets}
