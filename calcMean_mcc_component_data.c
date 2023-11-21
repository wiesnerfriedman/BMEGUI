/*
 * MATLAB Compiler: 4.13 (R2010a)
 * Date: Tue May 31 13:41:35 2011
 * Arguments: "-B" "macro_default" "-m" "-W" "main" "-T" "link:exe" "calcMean" 
 */

#include "mclmcrrt.h"

#ifdef __cplusplus
extern "C" {
#endif
const unsigned char __MCC_calcMean_session_key[] = {
    '3', '4', 'B', 'F', 'C', '4', 'E', 'E', 'E', '8', '8', '8', '0', '5', 'C',
    '7', '2', '2', '2', '4', '5', '8', '0', 'D', 'A', 'C', '5', 'C', 'C', '5',
    '7', '3', '2', 'C', 'D', 'F', '6', '6', '9', '3', '5', 'C', '5', 'A', 'B',
    '9', '0', 'A', '9', '8', 'A', '2', '5', '6', '7', 'F', '5', '4', 'A', '6',
    '7', 'E', 'E', '2', 'F', '6', 'A', '3', '8', '1', 'D', '7', 'F', '1', '1',
    'A', 'A', '4', '8', 'D', 'D', '8', 'E', '2', '4', '3', '3', 'E', '8', '5',
    'C', '3', 'C', '9', '6', '9', '2', '0', '7', '7', 'E', '5', 'F', '2', 'C',
    '0', '4', 'E', '8', 'B', 'C', 'D', '7', '5', '6', '4', 'F', '8', '1', '4',
    '4', 'F', '1', '3', '8', '2', 'E', 'A', '2', 'A', 'C', '3', '8', '9', '2',
    '8', '9', '8', '9', '6', '5', 'E', 'D', 'E', '4', '0', '2', '0', 'E', '2',
    'C', '5', 'B', 'A', 'F', '2', '2', '3', '6', '6', '3', 'D', '8', '1', '4',
    '7', '6', 'E', '8', 'C', 'E', 'F', '7', '9', 'B', 'F', 'D', 'D', 'B', 'B',
    '7', 'C', '5', '9', 'C', '4', '7', '8', 'D', '9', 'E', '4', '1', '2', '6',
    'A', '9', 'F', '6', '0', '4', '4', '9', 'A', '2', '1', '9', '7', '5', '6',
    '7', 'B', '2', '1', '7', '2', '1', '3', 'B', '7', '1', 'F', 'E', '4', 'D',
    'B', '9', '6', '7', 'F', '2', '5', '3', '1', '2', '2', 'B', 'D', '2', '0',
    'F', 'A', '7', '9', 'B', '8', '7', 'D', 'E', 'A', '7', '8', '5', '3', '2',
    'F', '\0'};

const unsigned char __MCC_calcMean_public_key[] = {
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

static const char * MCC_calcMean_matlabpath_data[] = 
  { "calcMean/", "$TOOLBOXDEPLOYDIR/", "$TOOLBOXMATLABDIR/general/",
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
    "Users/jat/Desktop/Desktop1-May2011/BMElibs/BMELIB2.0b_win64/iolib/",
    "Users/jat/Desktop/Desktop1-May2011/BMElibs/BMELIB2.0b_win64/genlib/" };

static const char * MCC_calcMean_classpath_data[] = 
  { "" };

static const char * MCC_calcMean_libpath_data[] = 
  { "" };

static const char * MCC_calcMean_app_opts_data[] = 
  { "" };

static const char * MCC_calcMean_run_opts_data[] = 
  { "" };

static const char * MCC_calcMean_warning_state_data[] = 
  { "off:MATLAB:dispatcher:nameConflict" };


mclComponentData __MCC_calcMean_component_data = { 

  /* Public key data */
  __MCC_calcMean_public_key,

  /* Component name */
  "calcMean",

  /* Component Root */
  "",

  /* Application key data */
  __MCC_calcMean_session_key,

  /* Component's MATLAB Path */
  MCC_calcMean_matlabpath_data,

  /* Number of directories in the MATLAB Path */
  41,

  /* Component's Java class path */
  MCC_calcMean_classpath_data,
  /* Number of directories in the Java class path */
  0,

  /* Component's load library path (for extra shared libraries) */
  MCC_calcMean_libpath_data,
  /* Number of directories in the load library path */
  0,

  /* MCR instance-specific runtime options */
  MCC_calcMean_app_opts_data,
  /* Number of MCR instance-specific runtime options */
  0,

  /* MCR global runtime options */
  MCC_calcMean_run_opts_data,
  /* Number of MCR global runtime options */
  0,
  
  /* Component preferences directory */
  "calcMean_20AE18FE79F1F20062DE45284E235AD0",

  /* MCR warning status data */
  MCC_calcMean_warning_state_data,
  /* Number of MCR warning status modifiers */
  1,

  /* Path to component - evaluated at runtime */
  NULL

};

#ifdef __cplusplus
}
#endif


