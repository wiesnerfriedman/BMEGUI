/*
 * MATLAB Compiler: 4.13 (R2010a)
 * Date: Wed Feb 22 15:03:14 2012
 * Arguments: "-B" "macro_default" "-m" "-W" "main" "-T" "link:exe"
 * "checkRiverNetworkContinuityTests.m" 
 */

#include "mclmcrrt.h"

#ifdef __cplusplus
extern "C" {
#endif
const unsigned char __MCC_checkRiverNetworkContinuityTests_session_key[] = {
    '8', '2', '9', '9', '6', 'D', '9', '8', '2', '7', '4', '0', 'C', 'E', 'B',
    '4', 'C', 'B', '3', '8', '4', 'E', '3', '5', '6', '7', '1', '5', '0', '3',
    'C', '0', '3', '9', '1', 'B', 'A', 'D', '8', 'B', 'A', 'C', '3', '8', 'E',
    'D', '6', 'C', 'D', 'E', '7', '3', '9', 'D', '5', 'B', 'F', 'D', 'A', 'D',
    '6', '3', 'A', 'B', '4', 'A', '1', '3', '0', '3', '4', '5', '9', '3', '4',
    '1', '1', 'A', 'A', '9', '1', '2', '2', '0', '8', 'A', 'A', 'A', '4', 'B',
    '2', 'A', '5', '1', 'A', '8', '2', '4', 'C', '4', 'E', '0', '2', '0', 'A',
    '5', 'F', 'D', 'E', '6', 'D', 'A', 'C', '6', '2', '1', '1', '3', '4', 'E',
    '2', '7', '7', '3', 'A', 'F', 'D', '5', '4', '9', 'B', '4', 'D', 'C', '2',
    '1', '3', '7', 'E', 'D', '4', '5', 'A', '0', '3', 'F', '5', 'F', 'A', 'F',
    '6', '5', '6', 'E', 'F', 'B', 'C', '9', 'A', 'D', '3', '6', '0', 'E', '0',
    'E', '5', '5', 'A', 'B', '5', 'A', 'C', 'E', '8', 'A', 'D', '3', 'A', '2',
    '9', '6', '1', '8', '3', 'E', '1', 'E', '3', 'F', 'D', '4', '8', '0', '8',
    '6', '4', 'C', '7', '3', 'E', '5', '5', '1', 'A', 'E', 'B', '1', 'B', 'F',
    'E', '5', '6', '9', 'D', '0', 'E', '3', '5', '1', '0', '2', 'D', 'F', '1',
    '0', '9', 'E', '6', '5', '4', '5', 'C', '1', '9', '4', '4', 'C', '5', '9',
    '9', 'F', '4', '9', '4', 'D', '1', '6', 'E', '0', '0', '9', '5', '2', '1',
    '3', '\0'};

const unsigned char __MCC_checkRiverNetworkContinuityTests_public_key[] = {
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

static const char * MCC_checkRiverNetworkContinuityTests_matlabpath_data[] = 
  { "checkRiverNe/", "$TOOLBOXDEPLOYDIR/", "$TOOLBOXMATLABDIR/general/",
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
    "toolbox/map/map/", "toolbox/map/mapdemos/", "toolbox/map/mapdisp/",
    "toolbox/map/mapformats/", "toolbox/map/mapproj/",
    "toolbox/shared/maputils/", "toolbox/shared/mapgeodesy/",
    "Users/jat/Desktop/Desktop1-May2011/BMElibs/BMELIB2.0b_win64/genlib/" };

static const char * MCC_checkRiverNetworkContinuityTests_classpath_data[] = 
  { "" };

static const char * MCC_checkRiverNetworkContinuityTests_libpath_data[] = 
  { "" };

static const char * MCC_checkRiverNetworkContinuityTests_app_opts_data[] = 
  { "" };

static const char * MCC_checkRiverNetworkContinuityTests_run_opts_data[] = 
  { "" };

static const char * MCC_checkRiverNetworkContinuityTests_warning_state_data[] = 
  { "off:MATLAB:dispatcher:nameConflict" };


mclComponentData __MCC_checkRiverNetworkContinuityTests_component_data = { 

  /* Public key data */
  __MCC_checkRiverNetworkContinuityTests_public_key,

  /* Component name */
  "checkRiverNetworkContinuityTests",

  /* Component Root */
  "",

  /* Application key data */
  __MCC_checkRiverNetworkContinuityTests_session_key,

  /* Component's MATLAB Path */
  MCC_checkRiverNetworkContinuityTests_matlabpath_data,

  /* Number of directories in the MATLAB Path */
  47,

  /* Component's Java class path */
  MCC_checkRiverNetworkContinuityTests_classpath_data,
  /* Number of directories in the Java class path */
  0,

  /* Component's load library path (for extra shared libraries) */
  MCC_checkRiverNetworkContinuityTests_libpath_data,
  /* Number of directories in the load library path */
  0,

  /* MCR instance-specific runtime options */
  MCC_checkRiverNetworkContinuityTests_app_opts_data,
  /* Number of MCR instance-specific runtime options */
  0,

  /* MCR global runtime options */
  MCC_checkRiverNetworkContinuityTests_run_opts_data,
  /* Number of MCR global runtime options */
  0,
  
  /* Component preferences directory */
  "checkRiverNe_A125F4FD63BBE3C3B940720F850004AF",

  /* MCR warning status data */
  MCC_checkRiverNetworkContinuityTests_warning_state_data,
  /* Number of MCR warning status modifiers */
  1,

  /* Path to component - evaluated at runtime */
  NULL

};

#ifdef __cplusplus
}
#endif


