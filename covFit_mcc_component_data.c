/*
 * MATLAB Compiler: 4.13 (R2010a)
 * Date: Tue May 31 13:42:05 2011
 * Arguments: "-B" "macro_default" "-m" "-W" "main" "-T" "link:exe" "covFit" 
 */

#include "mclmcrrt.h"

#ifdef __cplusplus
extern "C" {
#endif
const unsigned char __MCC_covFit_session_key[] = {
    'C', '2', 'D', '6', '3', '7', '4', '0', 'F', '1', '0', '1', '2', 'E', '1',
    '4', '4', '1', '2', 'E', '2', '7', '8', '0', '6', '0', '8', '9', 'D', '3',
    '4', '3', '5', '4', '2', '3', 'A', '9', 'A', '6', '0', 'A', '8', '3', 'A',
    '0', '6', '3', 'F', 'E', '0', '1', '4', 'F', '2', 'F', '0', 'B', 'C', 'E',
    '9', '1', '4', '0', 'A', '1', '9', '8', 'C', '0', 'D', 'C', '4', 'D', '9',
    '2', '6', '0', 'A', '4', '8', '6', 'E', '8', '6', 'D', 'F', '1', '2', '3',
    '4', '3', '4', '9', '1', '6', 'C', '4', '7', 'E', 'E', 'A', 'B', 'A', 'F',
    '5', '1', '9', '4', 'F', '5', 'E', '7', '3', 'B', 'E', '9', 'D', '4', '7',
    'A', 'E', 'D', '7', 'C', '9', 'A', 'B', 'A', '7', '2', 'B', '4', '2', '9',
    '3', 'E', '0', '2', '9', '7', 'F', 'E', '5', '4', '4', '3', 'E', 'E', 'E',
    '4', '7', '3', 'B', 'F', '3', 'B', '7', '6', 'E', '9', '3', 'D', '1', '1',
    'F', 'D', '8', '0', '4', 'F', '1', '1', '0', '2', '6', '5', 'A', '9', '9',
    '1', 'D', 'B', '6', '2', 'F', '7', '4', '0', 'C', '8', 'B', '0', 'D', '7',
    'C', '6', '3', '9', 'A', '6', '7', 'C', '7', '7', '9', '5', '6', '9', '0',
    '3', 'F', 'B', '9', '3', 'C', '8', 'D', '9', '5', '9', 'B', '3', '0', '8',
    '6', 'C', 'F', '0', 'A', '5', '6', '1', 'D', 'C', '3', 'F', '2', 'B', '5',
    '9', 'D', 'E', '8', '2', '8', '2', '6', '6', '3', 'C', '4', '1', 'E', 'F',
    '0', '\0'};

const unsigned char __MCC_covFit_public_key[] = {
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

static const char * MCC_covFit_matlabpath_data[] = 
  { "covFit/", "$TOOLBOXDEPLOYDIR/", "$TOOLBOXMATLABDIR/general/",
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

static const char * MCC_covFit_classpath_data[] = 
  { "" };

static const char * MCC_covFit_libpath_data[] = 
  { "" };

static const char * MCC_covFit_app_opts_data[] = 
  { "" };

static const char * MCC_covFit_run_opts_data[] = 
  { "" };

static const char * MCC_covFit_warning_state_data[] = 
  { "off:MATLAB:dispatcher:nameConflict" };


mclComponentData __MCC_covFit_component_data = { 

  /* Public key data */
  __MCC_covFit_public_key,

  /* Component name */
  "covFit",

  /* Component Root */
  "",

  /* Application key data */
  __MCC_covFit_session_key,

  /* Component's MATLAB Path */
  MCC_covFit_matlabpath_data,

  /* Number of directories in the MATLAB Path */
  39,

  /* Component's Java class path */
  MCC_covFit_classpath_data,
  /* Number of directories in the Java class path */
  0,

  /* Component's load library path (for extra shared libraries) */
  MCC_covFit_libpath_data,
  /* Number of directories in the load library path */
  0,

  /* MCR instance-specific runtime options */
  MCC_covFit_app_opts_data,
  /* Number of MCR instance-specific runtime options */
  0,

  /* MCR global runtime options */
  MCC_covFit_run_opts_data,
  /* Number of MCR global runtime options */
  0,
  
  /* Component preferences directory */
  "covFit_39DB99DA4B0AC5B78E117D04B34C3A3B",

  /* MCR warning status data */
  MCC_covFit_warning_state_data,
  /* Number of MCR warning status modifiers */
  1,

  /* Path to component - evaluated at runtime */
  NULL

};

#ifdef __cplusplus
}
#endif


