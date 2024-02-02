/*
 * MATLAB Compiler: 4.13 (R2010a)
 * Date: Wed Jun 29 12:11:47 2011
 * Arguments: "-B" "macro_default" "-m" "-W" "main" "-T" "link:exe"
 * "arcgridwrite" 
 */

#include "mclmcrrt.h"

#ifdef __cplusplus
extern "C" {
#endif
const unsigned char __MCC_arcgridwrite_session_key[] = {
    'C', '2', 'D', '7', '0', 'E', 'F', '6', 'D', '2', 'E', '2', '0', 'D', '4',
    'F', '8', 'E', 'A', '2', 'F', '0', '3', '8', 'A', '6', '9', 'C', 'E', '3',
    '9', '7', '6', 'F', '1', 'C', 'F', '9', 'C', '9', '6', '2', '9', 'F', '4',
    '5', '0', '6', '5', 'A', 'C', '0', '7', '1', '3', '5', 'D', '6', '7', '0',
    '2', '8', 'D', '6', 'A', '4', '9', '3', '7', 'D', '8', '8', 'D', 'A', '9',
    '0', '6', 'B', 'F', 'A', '1', '2', 'E', '5', '2', '2', 'B', '2', '9', '0',
    '2', '8', 'A', '2', 'E', '7', 'C', 'F', 'B', '6', 'A', '8', '8', 'B', '1',
    'A', '9', 'D', '2', '1', '9', '9', '6', '3', 'E', '5', '8', '3', '8', '7',
    '6', 'A', 'D', '5', '6', 'A', 'F', 'E', 'D', '0', '8', '5', 'C', '3', '8',
    'C', 'D', '4', 'D', '2', '5', '4', '9', '3', '8', 'F', '4', '3', '1', 'A',
    'B', 'D', 'B', '0', '0', '1', '2', 'F', '3', 'E', 'F', '8', '2', '5', '1',
    'B', '8', '7', '0', '7', '7', 'B', 'A', '0', '4', 'D', '6', 'C', '1', '8',
    '6', 'C', '0', '3', '0', 'B', '8', '3', '7', '3', '7', '9', '8', '8', '9',
    '3', 'F', 'A', '5', '3', '3', 'B', '5', '3', 'D', '7', 'F', '3', 'B', '2',
    '7', 'F', '3', '5', '7', 'B', 'C', 'A', '7', 'B', '1', '2', '5', '0', '5',
    '1', 'E', '8', '3', '5', '9', '7', '5', '3', '1', '2', 'C', '7', '9', '8',
    'B', 'F', '4', 'A', 'B', 'A', '3', '3', 'D', '0', '2', 'B', '8', '1', 'C',
    '8', '\0'};

const unsigned char __MCC_arcgridwrite_public_key[] = {
    '3', '0', '8', '1', '9', 'D', '3', '0', '0', 'D', '0', '6', '0', '9', '2',
    'A', '8', '6', '4', '8', '8', '6', 'F', '7', '0', 'D', '0', '1', '0', '1',
    '0', '1', '0', '5', '0', '0', '0', '3', '8', '1', '8', 'B', '0', '0', '3',
    '0', '8', '1', '8', '7', '0', '2', '8', '1', '8', '1', '0', '0', 'C', '4',
    '9', 'C', 'A', 'C', '3', '4', 'E', 'D', '1', '3', 'A', '5', '2', '0', '6',
    '5', '8', 'F', '6', 'F', '8', 'E', '0', '1', '3', '8', 'C', '4', '3', '1',
    '5', 'B', '4', '3', '1', '5', '2', '7', '7', 'E', 'D', '3', 'F', '7', 'D',
    'A', 'E', '5', '3', '0', '9', '9', 'D', 'B', '0', '8', 'E', 'E', '5', '8',
    '9', 'F', '8', '0', '4', 'D', '4', 'B', '9', '8', '1', '3', '2', '6', 'A',
    '5', '2', 'C', 'C', 'E', '4', '3', '8', '2', 'E', '9', 'F', '2', 'B', '4',
    'D', '0', '8', '5', 'E', 'B', '9', '5', '0', 'C', '7', 'A', 'B', '1', '2',
    'E', 'D', 'E', '2', 'D', '4', '1', '2', '9', '7', '8', '2', '0', 'E', '6',
    '3', '7', '7', 'A', '5', 'F', 'E', 'B', '5', '6', '8', '9', 'D', '4', 'E',
    '6', '0', '3', '2', 'F', '6', '0', 'C', '4', '3', '0', '7', '4', 'A', '0',
    '4', 'C', '2', '6', 'A', 'B', '7', '2', 'F', '5', '4', 'B', '5', '1', 'B',
    'B', '4', '6', '0', '5', '7', '8', '7', '8', '5', 'B', '1', '9', '9', '0',
    '1', '4', '3', '1', '4', 'A', '6', '5', 'F', '0', '9', '0', 'B', '6', '1',
    'F', 'C', '2', '0', '1', '6', '9', '4', '5', '3', 'B', '5', '8', 'F', 'C',
    '8', 'B', 'A', '4', '3', 'E', '6', '7', '7', '6', 'E', 'B', '7', 'E', 'C',
    'D', '3', '1', '7', '8', 'B', '5', '6', 'A', 'B', '0', 'F', 'A', '0', '6',
    'D', 'D', '6', '4', '9', '6', '7', 'C', 'B', '1', '4', '9', 'E', '5', '0',
    '2', '0', '1', '1', '1', '\0'};

static const char * MCC_arcgridwrite_matlabpath_data[] = 
  { "arcgridwrite/", "$TOOLBOXDEPLOYDIR/", "$TOOLBOXMATLABDIR/general/",
    "$TOOLBOXMATLABDIR/ops/", "$TOOLBOXMATLABDIR/lang/",
    "$TOOLBOXMATLABDIR/elmat/", "$TOOLBOXMATLABDIR/randfun/",
    "$TOOLBOXMATLABDIR/elfun/", "$TOOLBOXMATLABDIR/specfun/",
    "$TOOLBOXMATLABDIR/matfun/", "$TOOLBOXMATLABDIR/datafun/",
    "$TOOLBOXMATLABDIR/polyfun/", "$TOOLBOXMATLABDIR/funfun/",
    "$TOOLBOXMATLABDIR/sparfun/", "$TOOLBOXMATLABDIR/scribe/",
    "$TOOLBOXMATLABDIR/graph2d/", "$TOOLBOXMATLABDIR/graph3d/",
    "$TOOLBOXMATLABDIR/specgraph/", "$TOOLBOXMATLABDIR/graphics/",
    "$TOOLBOXMATLABDIR/uitools/", "$TOOLBOXMATLABDIR/strfun/",
    "$TOOLBOXMATLABDIR/imagesci/", "$TOOLBOXMATLABDIR/iofun/",
    "$TOOLBOXMATLABDIR/audiovideo/", "$TOOLBOXMATLABDIR/timefun/",
    "$TOOLBOXMATLABDIR/datatypes/", "$TOOLBOXMATLABDIR/verctrl/",
    "$TOOLBOXMATLABDIR/codetools/", "$TOOLBOXMATLABDIR/helptools/",
    "$TOOLBOXMATLABDIR/winfun/", "$TOOLBOXMATLABDIR/winfun/NET/",
    "$TOOLBOXMATLABDIR/demos/", "$TOOLBOXMATLABDIR/timeseries/",
    "$TOOLBOXMATLABDIR/hds/", "$TOOLBOXMATLABDIR/guide/",
    "$TOOLBOXMATLABDIR/plottools/", "toolbox/local/",
    "$TOOLBOXMATLABDIR/datamanager/", "toolbox/compiler/" };

static const char * MCC_arcgridwrite_classpath_data[] = 
  { "" };

static const char * MCC_arcgridwrite_libpath_data[] = 
  { "" };

static const char * MCC_arcgridwrite_app_opts_data[] = 
  { "" };

static const char * MCC_arcgridwrite_run_opts_data[] = 
  { "" };

static const char * MCC_arcgridwrite_warning_state_data[] = 
  { "off:MATLAB:dispatcher:nameConflict" };


mclComponentData __MCC_arcgridwrite_component_data = { 

  /* Public key data */
  __MCC_arcgridwrite_public_key,

  /* Component name */
  "arcgridwrite",

  /* Component Root */
  "",

  /* Application key data */
  __MCC_arcgridwrite_session_key,

  /* Component's MATLAB Path */
  MCC_arcgridwrite_matlabpath_data,

  /* Number of directories in the MATLAB Path */
  39,

  /* Component's Java class path */
  MCC_arcgridwrite_classpath_data,
  /* Number of directories in the Java class path */
  0,

  /* Component's load library path (for extra shared libraries) */
  MCC_arcgridwrite_libpath_data,
  /* Number of directories in the load library path */
  0,

  /* MCR instance-specific runtime options */
  MCC_arcgridwrite_app_opts_data,
  /* Number of MCR instance-specific runtime options */
  0,

  /* MCR global runtime options */
  MCC_arcgridwrite_run_opts_data,
  /* Number of MCR global runtime options */
  0,
  
  /* Component preferences directory */
  "arcgridwrite_CE945A5665615FFF6A6D27763E0FE929",

  /* MCR warning status data */
  MCC_arcgridwrite_warning_state_data,
  /* Number of MCR warning status modifiers */
  1,

  /* Path to component - evaluated at runtime */
  NULL

};

#ifdef __cplusplus
}
#endif


