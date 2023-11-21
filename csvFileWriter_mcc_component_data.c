/*
 * MATLAB Compiler: 4.13 (R2010a)
 * Date: Tue Jul 19 12:10:22 2011
 * Arguments: "-B" "macro_default" "-m" "-W" "main" "-T" "link:exe"
 * "csvFileWriter" 
 */

#include "mclmcrrt.h"

#ifdef __cplusplus
extern "C" {
#endif
const unsigned char __MCC_csvFileWriter_session_key[] = {
    '6', '4', 'E', '8', '1', 'F', 'C', '7', 'B', '8', 'C', '9', '8', 'D', 'A',
    '8', 'F', '6', '8', '8', 'B', 'F', '4', '5', 'A', '2', '1', 'A', '6', '5',
    '7', '7', 'A', '6', 'E', 'C', 'D', '0', '1', '1', 'A', '9', 'C', 'B', '0',
    'C', '9', '4', '2', '6', 'B', '6', '5', 'C', 'A', '2', '9', '5', '4', '2',
    'F', '6', 'D', '6', 'F', '9', '1', '1', '3', '7', '0', 'D', 'B', '9', '2',
    '1', '2', 'A', '9', 'C', 'A', '3', 'E', '1', '1', '5', 'A', 'B', '5', '7',
    '3', '3', '2', '0', 'F', 'E', 'A', '5', '0', '3', '0', '6', '3', '0', '2',
    'E', 'C', '0', '2', 'E', '5', '5', 'D', 'C', '0', 'B', 'A', '3', 'E', 'C',
    '2', 'B', '0', '6', 'B', 'C', 'C', '8', '2', '4', '2', '2', 'C', 'B', '4',
    'F', 'A', '6', '9', '2', 'A', '8', 'A', '4', '5', '4', 'C', '4', 'A', '2',
    '2', 'E', '9', 'E', 'C', '1', '8', 'C', '6', '8', 'A', 'F', 'C', 'D', 'A',
    '1', '2', 'A', 'A', '6', 'A', '3', '1', 'B', '1', 'A', 'A', 'F', '0', '9',
    '7', 'A', '1', 'F', '8', '0', 'D', 'F', '6', '8', '3', '4', '0', 'B', 'A',
    'B', '6', 'A', 'B', '0', 'A', '2', '2', 'E', '2', '5', '7', 'A', 'C', 'D',
    'A', 'A', '3', 'D', '0', '5', 'E', 'A', 'C', '3', '7', '2', 'A', '6', 'E',
    '1', '6', '0', 'F', '8', '0', 'E', 'F', 'C', 'F', '0', '9', 'D', 'E', 'D',
    '0', 'B', 'F', '2', '0', '6', '6', 'D', '0', '4', '5', 'E', 'E', '3', '8',
    '2', '\0'};

const unsigned char __MCC_csvFileWriter_public_key[] = {
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

static const char * MCC_csvFileWriter_matlabpath_data[] = 
  { "csvFileWrite/", "$TOOLBOXDEPLOYDIR/", "$TOOLBOXMATLABDIR/general/",
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

static const char * MCC_csvFileWriter_classpath_data[] = 
  { "" };

static const char * MCC_csvFileWriter_libpath_data[] = 
  { "" };

static const char * MCC_csvFileWriter_app_opts_data[] = 
  { "" };

static const char * MCC_csvFileWriter_run_opts_data[] = 
  { "" };

static const char * MCC_csvFileWriter_warning_state_data[] = 
  { "off:MATLAB:dispatcher:nameConflict" };


mclComponentData __MCC_csvFileWriter_component_data = { 

  /* Public key data */
  __MCC_csvFileWriter_public_key,

  /* Component name */
  "csvFileWriter",

  /* Component Root */
  "",

  /* Application key data */
  __MCC_csvFileWriter_session_key,

  /* Component's MATLAB Path */
  MCC_csvFileWriter_matlabpath_data,

  /* Number of directories in the MATLAB Path */
  39,

  /* Component's Java class path */
  MCC_csvFileWriter_classpath_data,
  /* Number of directories in the Java class path */
  0,

  /* Component's load library path (for extra shared libraries) */
  MCC_csvFileWriter_libpath_data,
  /* Number of directories in the load library path */
  0,

  /* MCR instance-specific runtime options */
  MCC_csvFileWriter_app_opts_data,
  /* Number of MCR instance-specific runtime options */
  0,

  /* MCR global runtime options */
  MCC_csvFileWriter_run_opts_data,
  /* Number of MCR global runtime options */
  0,
  
  /* Component preferences directory */
  "csvFileWrite_D1443EFE79593A0EE26861D243E08A16",

  /* MCR warning status data */
  MCC_csvFileWriter_warning_state_data,
  /* Number of MCR warning status modifiers */
  1,

  /* Path to component - evaluated at runtime */
  NULL

};

#ifdef __cplusplus
}
#endif


