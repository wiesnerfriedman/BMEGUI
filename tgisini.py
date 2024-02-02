"""TGIS tool: TGIS initial parameters
Name: tgisini.py
Version: 0.9
Last Modified: Jun 05, 2007
Developed by Y.Akita
"""

###  Constant setting  ###
# Default log zero setting
defLogZeroDiv = 25
defLogZeroVal = 0.001

# Number of bins
numHistBins = 40

# Colormap
strColorMap = "jet()"

# Plot quality standard: True(1) or False(0)
flgQstdPlot = 0
qstdVal = 0.388

# Projection setting flag
flgDataCoord = 0
flgProjCoord = 0
dataCoord = r"C:\Program Files\ArcGIS\Coordinate Systems\Geographic Coordinate Systems\North America\North American Datum 1983.prj"
projCoord = r"C:\Program Files\ArcGIS\Coordinate Systems\Projected Coordinate Systems\State Plane\NAD 1983 (Feet)\NAD 1983 StatePlane New Jersey FIPS 2900 (Feet).prj"

# Explanatory Plot Property
expPLHeader = "expPts"

# Sptl Mean Plot Property
sptlRawMeanHeader = "rawMean"
sptlSmMeanHeader = "smMean"

# Default Number of lags
defNumSptlLag = 10
defNumTempLag = 10

# BME estimation
defMaxDataPts = 10
defMaxSoftPts = 2
defNumEstPtsX = 20
defNumEstPtsY = 15
op1Val = 0
op3Val = 50000
op4Val = 0.001
op8Val = 2
orderVal = 0
defNumDispPtsX = 100
defNumDispPtsY = 80
defNumEstPtsT = 100
inclDataPtsT = 1

defScale = 0.1
defBufLen = 20

# BME point and raster file
bmePtHeader = "bmePt"
bmeRstHeader = "bmeRst"

### Set dictionary ###
userConst = {"numHistBins":numHistBins,
             "strColorMap":strColorMap,
             "flgQstdPlot":flgQstdPlot,
             "qstdVal":qstdVal,
             "flgDataCoord":flgDataCoord,
             "flgProjCoord":flgProjCoord,
             "dataCoord":dataCoord,
             "projCoord":projCoord,
             "expPLHeader":expPLHeader,
             "sptlRawMeanHeader":sptlRawMeanHeader,
             "sptlSmMeanHeader":sptlSmMeanHeader,
             "defMaxDataPts":defMaxDataPts,
             "defMaxSoftPts":defMaxSoftPts,
             "defNumEstPtsX":defNumEstPtsX,
             "defNumEstPtsY":defNumEstPtsY,
             "op1Val":op1Val,
             "op3Val":op3Val,
             "op4Val":op4Val,
             "op8Val":op8Val,
             "orderVal":orderVal,
             "defNumDispPtsX":defNumDispPtsX,
             "defNumDispPtsY":defNumDispPtsY,
             "defNumEstPtsT":defNumEstPtsT,
             "inclDataPtsT":inclDataPtsT,
             "bmePtHeader":bmePtHeader,
             "bmeRstHeader":bmeRstHeader,
             "defNumSptlLag":defNumSptlLag,
             "defNumTempLag":defNumTempLag,
             "defLogZeroDiv":defLogZeroDiv,
             "defLogZeroVal":defLogZeroVal,
             "defScale":defScale,
             "defBufLen":defBufLen}
