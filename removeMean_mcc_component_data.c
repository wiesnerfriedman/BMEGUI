/*
 * MATLAB Compiler: 4.13 (R2010a)
 * Date: Tue May 31 13:47:11 2011
 * Arguments: "-B" "macro_default" "-m" "-W" "main" "-T" "link:exe"
 * "removeMean" 
 */

#include "mclmcrrt.h"

#ifdef __cplusplus
extern "C" {
#endif
const unsigned char __MCC_removeMean_session_key[] = {
    '6', '0', '5', '8', '9', '8', '3', 'B', 'B', '0', 'D', '9', 'D', '2', '1',
    'F', '8', '2', '8', '4', 'D', '0', '0', '1', '1', '0', '5', 'B', '5', '9',
    'A', 'B', '8', 'D', 'E', '7', '5', '6', 'A', '1', '1', 'E', 'E', '1', 'A',
    '0', '2', '1', 'E', 'B', '3', '2', 'D', 'C', 'A', 'A', '7', '1', 'E', '2',
    '8', '5', 'B', '3', '9', 'E', 'C', '9', '5', 'F', 'E', '9', '0', '5', 'D',
    '2', 'E', 'F', '9', '4', '7', '2', 'A', 'F', '3', '9', '5', '7', 'D', 'E',
    '9', '0', 'C', 'F', '0', 'F', 'A', '9', '5', '9', '5', '3', '8', '6', 'F',
    '2', '6', 'A', '9', 'A', 'D', '6', 'A', '2', 'F', '5', 'A', '9', '5', 'A',
    '4', 'C', '5', 'D', 'B', '8', '7', '5', 'A', '4', '0', '7', '3', 'C', 'F',
    '2', 'C', 'B', 'C', '8', '9', '0', '2', 'E', '1', '9', '9', '3', '5', '5',
    '3', 'D', 'C', '3', 'A', '2', '6', '0', '8', '5', 'C', '0', '2', '1', '5',
    '7', 'D', 'D', '4', '6', '2', '5', 'C', 'C', '2', '2', '8', 'D', 'B', '7',
    'B', '0', 'B', '2', 'F', '6', '6', '9', 'F', 'B', '2', 'E', '8', '6', '8',
    '9', '9', '1', 'F', '5', '7', 'B', '0', '4', '4', 'C', 'C', 'D', 'A', '8',
    '4', '2', '2', '8', '5', 'B', 'E', '1', '3', '0', 'C', '0', 'C', '6', '7',
    'F', '9', 'D', 'F', '1', '4', 'A', '6', 'A', '3', '8', 'F', 'B', '7', 'A',
    'A', 'F', 'D', '4', 'B', 'B', '9', '6', '7', '7', 'F', '5', '9', '3', 'A',
    '1', '\0'};

const unsigned char __MCC_removeMean_public_key[] = {
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

static const char * MCC_removeMean_matlabpath_data[] = 
  { "removeMean/", "$TOOLBOXDEPLOYDIR/", "$TOOLBOXMATLABDIR/general/",
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
    "$TOOLBOXMATLABDIR/datamanager/", "toolbox/compiler/",
    "Users/jat/Desktop/Desktop1-May2011/BMElibs/BMELIB2.0b_win64/statlib/" };

static const char * MCC_removeMean_classpath_data[] = 
  { "" };

static const char * MCC_removeMean_libpath_data[] = 
  { "" };

static const char * MCC_removeMean_app_opts_data[] = 
  { "" };

static const char * MCC_removeMean_run_opts_data[] = 
  { "" };

static const char * MCC_removeMean_warning_state_data[] = 
  { "off:MATLAB:dispatcher:nameConflict" };


mclComponentData __MCC_removeMean_component_data = { 

  /* Public key data */
  __MCC_removeMean_public_key,

  /* Component name */
  "removeMean",

  /* Component Root */
  "",

  /* Application key data */
  __MCC_removeMean_session_key,

  /* Component's MATLAB Path */
  MCC_removeMean_matlabpath_data,

  /* Number of directories in the MATLAB Path */
  40,

  /* Component's Java class path */
  MCC_removeMean_classpath_data,
  /* Number of directories in the Java class path */
  0,

  /* Component's load library path (for extra shared libraries) */
  MCC_removeMean_libpath_data,
  /* Number of directories in the load library path */
  0,

  /* MCR instance-specific runtime options */
  MCC_removeMean_app_opts_data,
  /* Number of MCR instance-specific runtime options */
  0,

  /* MCR global runtime options */
  MCC_removeMean_run_opts_data,
  /* Number of MCR global runtime options */
  0,
  
  /* Component preferences directory */
  "removeMean_4D78993A2B5E9414A31B27CB403A634C",

  /* MCR warning status data */
  MCC_removeMean_warning_state_data,
  /* Number of MCR warning status modifiers */
  1,

  /* Path to component - evaluated at runtime */
  NULL

};

#ifdef __cplusplus
}
#endif


