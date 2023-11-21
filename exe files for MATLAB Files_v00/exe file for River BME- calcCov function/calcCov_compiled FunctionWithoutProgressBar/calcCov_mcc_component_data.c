/*
 * MATLAB Compiler: 4.13 (R2010a)
 * Date: Thu May 19 23:23:37 2011
 * Arguments: "-B" "macro_default" "-m" "-W" "main" "-T" "link:exe" "calcCov.m" 
 */

#include "mclmcrrt.h"

#ifdef __cplusplus
extern "C" {
#endif
const unsigned char __MCC_calcCov_session_key[] = {
    '3', '2', '2', '5', 'E', '7', '2', '1', '6', 'C', 'A', '1', '4', 'A', 'A',
    '7', 'D', 'B', '5', 'C', '9', 'E', '3', '7', 'E', '7', '8', '3', '1', '7',
    'C', '4', '2', 'F', 'D', 'D', 'C', '8', 'A', '6', 'C', '4', 'C', 'D', '6',
    '1', '1', '9', '2', 'A', '5', '1', 'B', 'C', '0', 'C', '7', '2', 'F', '8',
    '2', 'E', '2', '8', '5', '2', '2', '6', 'E', 'B', '3', 'B', '8', 'F', '5',
    'E', 'A', '5', '9', '9', '2', 'B', '2', '5', 'D', '6', '1', 'D', '7', '4',
    'C', 'C', '6', '1', 'A', 'D', 'C', '3', 'A', '2', '3', '9', '7', '1', '7',
    '1', 'A', 'B', '8', '9', '6', '6', 'D', 'B', '6', '1', '2', 'D', 'F', 'E',
    '6', '8', 'B', 'B', '9', '8', '6', '8', '9', 'A', 'B', '0', 'F', '7', '1',
    '1', '3', '9', '2', '9', '2', 'C', '8', '2', '2', '9', '2', 'B', '4', '1',
    'A', '7', '6', '1', 'C', '3', 'A', '7', 'F', '4', 'D', 'F', 'D', '4', '8',
    '4', '2', '8', '7', 'C', '5', 'C', '3', '0', '9', '2', 'D', 'A', 'D', 'A',
    'B', '8', '0', 'B', '5', 'B', 'E', '7', '0', 'D', '3', '1', '9', '8', '7',
    'F', '2', '2', 'E', '3', '1', '2', '3', '2', '7', 'C', '1', '5', '3', '2',
    '4', '8', 'F', '8', 'C', 'A', 'A', '4', '7', '0', 'E', '8', '7', '0', 'C',
    'E', 'F', '0', '8', 'F', 'F', '5', '6', 'F', '0', '6', 'B', '3', '4', '8',
    '3', '4', '7', '4', '6', '0', 'F', '4', 'F', '5', 'F', '1', 'F', '1', '0',
    'A', '\0'};

const unsigned char __MCC_calcCov_public_key[] = {
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

static const char * MCC_calcCov_matlabpath_data[] = 
  { "calcCov/", "$TOOLBOXDEPLOYDIR/", "$TOOLBOXMATLABDIR/general/",
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

static const char * MCC_calcCov_classpath_data[] = 
  { "" };

static const char * MCC_calcCov_libpath_data[] = 
  { "" };

static const char * MCC_calcCov_app_opts_data[] = 
  { "" };

static const char * MCC_calcCov_run_opts_data[] = 
  { "" };

static const char * MCC_calcCov_warning_state_data[] = 
  { "off:MATLAB:dispatcher:nameConflict" };


mclComponentData __MCC_calcCov_component_data = { 

  /* Public key data */
  __MCC_calcCov_public_key,

  /* Component name */
  "calcCov",

  /* Component Root */
  "",

  /* Application key data */
  __MCC_calcCov_session_key,

  /* Component's MATLAB Path */
  MCC_calcCov_matlabpath_data,

  /* Number of directories in the MATLAB Path */
  39,

  /* Component's Java class path */
  MCC_calcCov_classpath_data,
  /* Number of directories in the Java class path */
  0,

  /* Component's load library path (for extra shared libraries) */
  MCC_calcCov_libpath_data,
  /* Number of directories in the load library path */
  0,

  /* MCR instance-specific runtime options */
  MCC_calcCov_app_opts_data,
  /* Number of MCR instance-specific runtime options */
  0,

  /* MCR global runtime options */
  MCC_calcCov_run_opts_data,
  /* Number of MCR global runtime options */
  0,
  
  /* Component preferences directory */
  "calcCov_39463D1D218B18173168A6B8E1CF5EC0",

  /* MCR warning status data */
  MCC_calcCov_warning_state_data,
  /* Number of MCR warning status modifiers */
  1,

  /* Path to component - evaluated at runtime */
  NULL

};

#ifdef __cplusplus
}
#endif


