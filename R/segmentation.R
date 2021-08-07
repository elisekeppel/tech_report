# Last Modified: Oct-29-2019
# Written by: Brianna Wright (Brianna.Wright@dfo-mpo.gc.ca)
# in R version 3.5.3 (2019-03-11)

#=========================================== STEP ONE: CREATE LINES FROM EFFORT POINTS ============================================#

# Note: this only needs to be RUN ONCE on the unstratified Effort Points file (UNLESS transect names have changed since the last run).
# But note that Transect names will have changed IF STRATIFICATION TOOL RESULTED IN CHOPPING OF TRANSECTS.
# If transect names have not changed, then you can skip down to STEP TWO (Segmentation).

# Clear R environment:
rm(list=ls(all= TRUE)) ; ls()

# Load packages:
library(sp)
library(rgeos)
library(spatstat)
library(dplyr)
library(rgdal)
library(tmaptools)
year <- 2021
month <- 07
# Effort & Sightings data have been stratified for DSM into "NorthernShelf_Bioregion", "GeorgiaStrait_JuanDeFuca" and "offshore_tully" (Oct-11-2019).
# Stratification in this case resulted in a few "chopped up" transects/ONSEQ_ID names.

# For cases in which Effort stratification has resulted in "chopped" pieces, you will want to run this on the STRATIFIED effort.
# This is because transects that cross strata boundaries will have been divided into two new transects, and thus should be
# considered separate lines that will then be segmented independently. This will ensure that no segments cross model strata
# boundaries and are only assigned to one stratum.

# In this step, we want to create lines that are continous except in OFF-effort portions AND when the transect
# number changes. We can use the GPSIndex values to decide if Effort points should be linked up into the current 
# line or if that line should end and a new line should be started using the next Effort point.

# Strategy:
# Create a dummy variable called lag.GPSIndex that consists of the 1:nrow(GPSIndex) data. The first lagged GPSIndex value of each survey/ship will be NA.
# Create a second dummy variable that determines the difference between the lagged and unlagged GPS indices (i.e. are the GPS indices continuous between successive points?)
# Generate a line.code based on which GPS points are consecutive (AND based on changes in transect ID) ==> use the rle function in R

# e.g. GPSIndex   lag.GPSIndex  diff.GPS    line.code
#      1          NA            NA          1       
#      2          1             1           1         
#      3          2             1           1         
#      7          3             4           2    
#      8          7             1           2

# Generate lines by splitting Effort points based on line.code values
# Apply segmentation on these lines

### Read in Stratified Effort points data:
# setwd("C:/Users/WrightBri/Documents/PRISMM/ANALYSIS")
# Effort<-read.table("PRISMM_dataEffort_StrataAssigned.txt", sep="\t", header=TRUE, stringsAsFactors=FALSE, na.strings=c("","NA","NaN"))
Effort<-read.table(file.path("OUTPUT FILES","dataEffort table", paste0("dataEffortcemore_2021jul", ".txt")), sep="\t", header=TRUE, stringsAsFactors=FALSE, na.strings=c("","NA","NaN"))

# Sort data by vessel AND THEN by time stamp
# EK edit
# Effort$GpsT_PDT<-as.POSIXct(Effort$GpsT_PDT, format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")
Effort$GpsT.UTC<-as.POSIXct(Effort$GpsT.UTC, format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")
# EK edit
# index<-with(Effort, order(Vessel, GpsT_PDT))
index<-with(Effort, order(Vessel, GpsT.UTC))
Effort<-Effort[index,]
rownames(Effort)<-1:length(Effort$GPSIndex)
# Compute new columns: lag GPSIndex by 1 (i.e. lag.GPSIndex is the GPSIndex immediately BEFORE the current GPSIndex), and difference between current and previous GPSIndex
Effort <- Effort %>%
  mutate(lag.GPSIndex=dplyr::lag(GPSIndex, n=1, default=NA), diff.GPS=GPSIndex-lag.GPSIndex) %>%
  as.data.frame()
# Determine which GPS Indices are continuous runs of Effort
runs<-rle(Effort$diff.GPS)
rle.runs<-data.frame(unclass(runs))
rle.runs$code<-NA

### Assign line codes based on continuity of GPS Indices:
# If lengths>=1 and values=1, this indicates a new continuous line of uninterrupted GPSIndex points
# If lengths=1 and values=NA OR lengths=1 and values<0, this indicates the first point of each survey, by vessel (code as part of the following line)
# If lengths=1 and values=0, this is the first point on a new zig-zag transect (code as part of the next line) - its GPSIndex will be the same as the last point on the prior zig-zag
# If lengths=1 and values>1, this indicates the first point of a new line (code to the following line)
# First, assign codes for runs of consecutive points
rle.runs[which(rle.runs$lengths>=1 & rle.runs$values==1),]$code <- seq(from=1, to=length(rle.runs[which(rle.runs$lengths>=1 & rle.runs$values==1),]$code), by=1)
# Assign codes for final points on zig-zag transects, leading points of new lines, and NA points (last point in both TA and TU surveys)
for (i in 1:length(rle.runs$code)){
  if(rle.runs$lengths[i]==1 && is.na(rle.runs$values[i])){
    rle.runs$code[i]<-rle.runs$code[i+1]
  } else
    if(rle.runs$lengths[i]==1 && rle.runs$values[i]<0){
      rle.runs$code[i]<-rle.runs$code[i+1]
    } else
      if(rle.runs$lengths[i]==1 && rle.runs$values[i]==0){
        rle.runs$code[i]<-rle.runs$code[i+1]
      } else
        if(rle.runs$lengths[i]==1 && rle.runs$values[i]>1){
          rle.runs$code[i]<-rle.runs$code[i+1]
        } else
          if(!is.na(rle.runs$code[i])){
            next
          }
}

# Create a vector of line.codes the same length as number of rows found in the Effort data.frame, using lengths as a multiplier of line.code values
rle<-cbind.data.frame(rle.runs$lengths, rle.runs$code)
colnames(rle)<-c("lengths","values")
inv<-inverse.rle(rle)
Effort$line.code<-inv

# Look for line.codes that contain more than one transect name
transects <- Effort %>%
  dplyr::group_by(line.code) %>%
  # summarise(num.trans=n_distinct(Transect)) # EK edit
  dplyr::summarise(num.trans=n_distinct(Final.T.ID))

transect.list<-as.data.frame(filter(transects, num.trans>1))
# If any line codes contain more than one transect, throw an Error and take a closer look at them to see why this is the case.
if(dim(transect.list)[1]>0){
  stop(paste0("A line code (", toString(as.character(transect.list$line.code)), ") contains more than one transect. Examine and re-code so that each transect has a unique line code."), call.=FALSE)
}

# Examine line.codes that contain more than one transect 
# line.code 32 - this seems to be a transit line that runs seamlessly into a true transect without breaks
line.codes<-transect.list$line.code
for (i in 1:length(line.codes)){
  max.line.code<-max(Effort$line.code, na.rm=TRUE)
  line.code<-transect.list$line.code[i]
  # transects<-unique(Effort[which(Effort$line.code==transect.list$line.code[i]),]$Transect) # EK edit
  # Effort[which(Effort$line.code==line.code & Effort$Transect==transects[2]),]$line.code <- max.line.code + 1
  transects<-unique(Effort[which(Effort$line.code==transect.list$line.code[i]),]$Final.T.ID)
  Effort[which(Effort$line.code==line.code & Effort$Final.T.ID==transects[2]),]$line.code <- max.line.code + 1
}

# Make second parts/transects of line.codes = 32, 126 and 131 into new line codes to reflect the new transect number


# Run this check again: Select and re-run Lines 103 through 111, above ==> should no longer produce an Error message if the fix has been applied correctly

### Generate lines using line.code as the value with which to link points together
# Make line.code a factor
Effort$line.code<-as.factor(Effort$line.code)
# Make Effort points into a SpatialPointsDataFrame (unprojected)
Epoints<-Effort
coordinates(Epoints)<-c("Longitude","Latitude")
proj4string(Epoints)<-CRS("+proj=longlat")
# Split SpatialPointsDataFrame by line.code and convert to a list of lines, with each list object representing a unique line.code & its corresponding coordinates
x <- lapply(X=split(Epoints, Epoints$line.code), function(X) Lines(list(Line(coordinates(X))), ID=X$line.code[1]))
# Convert the list of lines to a SpatialLines object (unprojected)
l <- SpatialLines(x, proj4string=CRS("+proj=longlat"))
# Compute line.code total lengths in metres (summed Great Circle distances between successive GPSIndex points)
l.length<-(SpatialLinesLengths(l, longlat=TRUE))*1000
range(l.length) #[1] 158.6748 164553.7803 m
l.length<-as.data.frame(l.length)
l.length$ID <- sapply(1:length(l), function(i) l@lines[[i]]@ID)
colnames(l.length)<-c("lc.len.m","line.code")
l.length$line.code<-as.numeric(l.length$line.code)
# Assemble data to be associated with lines based on line.codes
# line.code, Vessel, min.GPSIndex, max.GPSIndex, Sub.Trans, Transect, Iteration
line.data <- Effort %>%
  dplyr::group_by(line.code) %>%     # TO DO if dplyr functions through an error, edit so 'dplyr::' before dplyr functions!
  dplyr::summarise(vessel=unique(Vessel),
            min.GPSInd=min(GPSIndex),
            max.GPSInd=max(GPSIndex),
            # sub.Trans=unique(Sub.Trans), # EK edit
            # Transect=unique(Transect),
            Transect=unique(Final.T.ID), # EK edit
            Iteration=unique(iteration)
            ) %>%
  as.data.frame()
line.data$line.code<-as.numeric(as.character(line.data$line.code))
line.data<-left_join(line.data, l.length, by="line.code")
# Convert from SpatialLines to SpatialLinesDataFrame and append line.data information
sldf <- SpatialLinesDataFrame(l, line.data, match.ID="line.code")
# Save SpatialLinesDataFrame as ESRI shapefile
writeOGR(sldf, dsn=getwd(), layer="cemore_EffortLines_AllSurveys_Unsegmented", driver="ESRI Shapefile", overwrite_layer=TRUE)
# Save Effort points file with line.codes
Effort<-dplyr::select(Effort, -c(lag.GPSIndex, diff.GPS))
# write.table(Effort, "PRISMM_dataEffort_AllSurveys_LinesCoded.txt", sep="\t", quote=FALSE, row.names=FALSE)
write.table(Effort, file = paste0("cemore_dataEffort_", year, "_", month,  "_LinesCoded.txt"), sep="\t", quote=FALSE, row.names=FALSE)


#========================================== STEP TWO: SEGMENT EFFORT LINES ==============================================#

# Note: Set target SEGMENT LENGTH (m) in build_segs() function, see Line 190 of this file

# Clear R environment:
rm(list=ls(all= TRUE)) ; ls()
year <- 2021
month <- 07
# Load packages:
library(sp)
library(rgeos)
library(spatstat)
library(dplyr)
library(rgdal)
library(geosphere)

# Read in Effort lines
# setwd("C:/Users/WrightBri/Documents/PRISMM/ANALYSIS")
EffortLines<-readOGR(dsn=getwd(), layer="cemore_EffortLines_AllSurveys_Unsegmented")
# Get data from lines
LinesData<-as.data.frame(EffortLines@data)
colnames(LinesData)<-c("line.code","vessel","minGPS","maxGPS",#"subTransect", # EK edit
                      "Transect",
                      "Iteration",
                      "length.m")
# Convert line.code from a factor ==> character ==> numeric
LinesData$line.code<-as.numeric(as.character(LinesData$line.code))

### FUNCTION to determine number of segments per line.code based on target segment length,
# and compute the lengths of each segment. Specify target segment length in metres using "seglen" argument.
# This function was built using code adapted from segchopr_2018_04_30.R by Elizabeth Becker and Karen Forney.
# See also: SegmentChoppingGraphics.ppt for more details on the NOAA methodology used here.
#-------------------------------------------------------------------------------------------------#
build_segs <- function(line.code, tot.length){
  # Set the TARGET SEGMENT LENGTH (all lengths in this function are given in metres)
  seglen <- 5000
  # Set the maximum proportion of leftover segment length to randomly add to other segments
  segtol <- 0.5*seglen
  # Set minimum segment length (m) to assign to segments of true length=0
  minsegdist <- 100
  # If length of line.code is 0, assign a minimum segment length
  if(tot.length==0){
    num.segs <- 1
    seg.lengths <- minsegdist
  } else
    # If length of line.code is smaller than the target segment length
    if(tot.length < seglen){
      num.segs <- 1
      seg.lengths <- tot.length
    } else {
      # If length of line code is greater than the target segment length
      num.segs <- floor(tot.length/seglen)
      leftover <-((tot.length/seglen) - floor(tot.length/seglen))*seglen
      # If leftover bit is greater than or equal to the maximum portion to add to other segments
      if(leftover >= segtol){
        num.segs <- num.segs + 1 # leftover bit becomes its own segment
        extrabit <- leftover
        # If leftover bit is less than the maximum portion to add to other segments
      } else {
        extrabit <- seglen + leftover
      }
      seg.lengths <- 0
      seg.lengths[1:num.segs] <- seglen
      # Randomly choose a segment to absorb the extrabit
      randnum <- runif(1,0,1) # Randomly generate one number between 0 and 1
      randpick <- floor(randnum*num.segs)+1
      seg.lengths[randpick] <- extrabit
    }
  # Calculate cumulative length of segments over the total line
  cum.seg.lengths <- cumsum(seg.lengths)
  # Calculate segment ID numbers
  seg.IDs <- seq(from=1, to=num.segs, by=1)
  # Calculate lower values of segment length range
  lengths.lower <- cum.seg.lengths - seg.lengths
  # Return results as a list
  results <- list(line=line.code, line.length=tot.length, num.segs=num.segs, seg.lengths=seg.lengths, cum.seg.lengths=cum.seg.lengths, lengths.lower=lengths.lower, segIDs=seg.IDs)
  return(results)
}
#-------------------------------------------------------------------------------------------------#

### Use mapply to run the build_segs function over every element of LinesData and return a list of Lines
# that includes for each line: line code, total line length, number of segments, and all segment lengths
segments <- Map(build_segs, LinesData$line.code, LinesData$length.m)
# Save segments R object
saveRDS(segments, file=paste0("cemore_Segments_", year, "_", month, ".txt", version=3))
#test<-readRDS("PRISMM_Segments.txt") # Test re-import of list
# Make segments into a long format data.frame
SEGS<-plyr::rbind.fill(lapply(segments,as.data.frame))
SEGS<-dplyr::select(SEGS, -lengths.lower)
colnames(SEGS)<-c("line.code","line.len","total.segs","seg.len", "cum.seg.len","seg.num")
SEGS$seg.ID <- SEGS %>% dplyr::group_indices(line.code, seg.num)
# Save long format segments data.frame
write.table(SEGS, paste0("cemore_SegmentAssignedLengths_LongFormat_", year, "_", month, ".txt"), sep="\t", quote=FALSE, row.names=FALSE)
# Check that sum(seg.len) equals the total line.len for each line.code, to 2 decimal places
line.lengths <- SEGS %>%
  dplyr::group_by(line.code) %>%
  dplyr::summarise(line.len.sum=sum(seg.len), line.len=unique(line.len)) %>%
  as.data.frame()
for (i in 1:nrow(line.lengths)){
  if(round(line.lengths$line.len.sum[i], digits=2)!=round(line.lengths$line.len[i], digits=2)){
    stop(paste0("A line code (", toString(as.character(line.lengths$line.code)), ") has a total length that does equal the sum of its segments. Examine and fix."), call.=FALSE)
  }
}


### Apply segment lengths to Effort points to designate a segment ID for each point
# Read in Effort points
Effort<-read.table(paste0("cemore_dataEffort_", year, "_", month, "_LinesCoded.txt"), sep="\t", header=TRUE, stringsAsFactors=FALSE, na.strings=c("","NA","NaN"))
# Lag position (lat/lon) by one time-step
Effort <- Effort %>% 
  dplyr::group_by(line.code) %>%
  dplyr::mutate(lag.Lat=lag(Latitude, n=1, default=NA), lag.Lon=lag(Longitude, n=1, default=NA)) %>%
  as.data.frame()
# Assign position 1 to current row lat/lon, and position 2 to lagged lat/lon
pos1<-as.matrix(cbind.data.frame(Effort$Longitude, Effort$Latitude))
colnames(pos1)<-c("Longitude","Latitude")
pos2<-as.matrix(cbind.data.frame(Effort$lag.Lon, Effort$lag.Lat))
colnames(pos2)<-c("Longitude","Latitude")
# Compute Great-Circle Distance between consecutive Effort GPS points in metres (Haversine formula)
Effort <- Effort %>%
  dplyr::mutate(distFromPrev.m=distHaversine(pos1, pos2, r=6378137)) # r=radius of the Earth in metres
# drop unnecessary columns
Effort <- dplyr::select(Effort, -c(lag.Lat, lag.Lon)) 
# Compute cumulative distance covered for all the Effort GPS points within each line (i.e., by line.code)
sums <- Effort %>%
  dplyr::group_by(line.code) %>%
  dplyr::filter(!is.na(distFromPrev.m)) %>%
  dplyr::mutate(cumDist.m=cumsum(distFromPrev.m)) %>%
  as.data.frame()
Effort$cumDist.m<-NA
Effort[!is.na(Effort$distFromPrev.m),]$cumDist.m<-sums$cumDist.m
# Make cumulative distances = 0 for first points in each line.code
Effort[is.na(Effort$cumDist.m),]$cumDist.m<-0


### FUNCTION to assign segment IDs by segment distance intervals within each line code
#--------------------------------------------------------------------------------------------------------------------#
identify_segs <- function(line, cumDist.m){
  # Make data.frame of Segment IDs, lower and upper bounds of length range for each from the list of segments
  segs <- cbind.data.frame(segments[[line]]$segIDs, segments[[line]]$lengths.lower, segments[[line]]$cum.seg.lengths)
  colnames(segs)<-c("ID","lower","upper")
  # Designate segment IDs for the chunk:
  SegmentID <- findInterval(cumDist.m, segs$lower, rightmost.closed = TRUE, left.open=TRUE)
  # Return results
  return(SegmentID)
}
#--------------------------------------------------------------------------------------------------------------------#

# Apply function over rows of Effort data using mapply ... this will take a minute or two
Effort <- Effort %>%
  mutate(Seg.Num = mapply(identify_segs, Effort$line.code, Effort$cumDist.m))

### Make unique Segment codes (i.e. segment codes that do not repeat beginning at 1 again for each unique line.code)
Effort$Seg.ID <- Effort %>% group_indices(line.code, Seg.Num)

### Note: Segments will be too short if we build segment lines with the existing effort points b/c the end of one segment will not be connected to the beginning of the next
# To solve this, we need to make a dummy point in the Effort file that is the duplicate of every segment start point (i.e. cumDist.m=0), but assign it to be the
# end point of the segment immediately prior. This dummy point becomes unnecessary if the next segment is part of a new line.code.
# To create the dummy variable, take the first row from every group of Seg.ID
dummy <- Effort %>%
  dplyr::group_by(Seg.ID) %>%
  slice(1) %>%
  dplyr::ungroup() %>%
  as.data.frame()
# Remove the first point from each Vessel group (i.e. survey) ==> as there will be no segment preceding it
dummy <- dummy %>%
  group_by(Vessel) %>%
  slice(2:n()) %>%
  ungroup() %>%
  as.data.frame()
dummy$Seg.Num<-NA
dummy$Seg.ID<-NA
# Add dummy variable back into Effort data
Effort<-rbind.data.frame(Effort,dummy)
# Order Effort by Vessel and THEN timestamp
Effort$GpsT.UTC<-as.POSIXct(Effort$GpsT.UTC, format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")
index<-with(Effort, order(Vessel, GpsT.UTC))
Effort<-Effort[index,]
rownames(Effort)<-1:length(Effort$GPSIndex)
# Assign Seg.ID for dummy points to be equal to the ID of the previous segment, unless the line.code has changed compared to the previous point
for (i in 1:length(Effort$Seg.ID)){
  if(is.na(Effort$Seg.ID[i]) && Effort$line.code[i]==Effort$line.code[i-2]){
    Effort$Seg.ID[i] <- Effort$Seg.ID[i-2]
    Effort$Seg.Num[i] <- Effort$Seg.Num[i-2]
  } else
    next
}
# Remove rows with Seg.ID==NA (not needed because this indicates a change in line.code)
# i.e. the last point of one segment IS NOT the same as the first point of the next segment because there is a break in the line continuity, indicated by new line.code
Effort <- dplyr::filter(Effort, !is.na(Seg.ID))
# Re-order data & re-name rows to reflect the new order
index<-with(Effort, order(Seg.ID, GpsT.UTC))
Effort<-Effort[index,]
rownames(Effort)<-1:length(Effort$GPSIndex)

# Write new Effort file
write.table(Effort, paste0("cemore_EffortLines_", year, "_", month, "_Segmented.txt"), sep="\t", quote=FALSE, row.names=FALSE)

### Make segment lines based on Seg.ID and save as Shapefile
# Make Seg.ID a factor
Effort$Seg.ID<-as.factor(Effort$Seg.ID)
# Make Effort points into a SpatialPointsDataFrame (unprojected)
Epoints<-Effort
coordinates(Epoints)<-c("Longitude","Latitude")
proj4string(Epoints)<-CRS("+proj=longlat")
# Split SpatialPointsDataFrame by Seg.ID and convert to a list of lines, with each list object representing a unique Seg.ID & its corresponding coordinates
x <- lapply(X=split(Epoints, Epoints$Seg.ID), function(X) Lines(list(Line(coordinates(X))), ID=X$Seg.ID[1]))
# Convert the list of lines to a SpatialLines object (unprojected)
l <- SpatialLines(x, proj4string=CRS("+proj=longlat"))
# Compute Seg.ID total lengths in metres (summed Great Circle distances between successive GPSIndex points ==> *1000 to convert from km to m)
l.length<-(SpatialLinesLengths(l, longlat=TRUE))*1000
range(l.length) #[1]  158.6748 7569.9421
l.length<-as.data.frame(l.length)
l.length$ID <- sapply(1:length(l), function(i) l@lines[[i]]@ID)
colnames(l.length)<-c("Seg.len.m","Seg.ID")
# Assemble data to be associated with segments based on segment IDs
# line.code, Vessel, min.GPSIndex, max.GPSIndex, Sub.Trans, Transect, Iteration
seg.data <- Effort %>%
  group_by(Seg.ID) %>%
  summarise(vessel=unique(Vessel),
            min.GPSInd=min(GPSIndex),
            max.GPSInd=max(GPSIndex),
            sub.Trans=unique(Sub.Trans),
            Transect=unique(Transect),
            Iteration=unique(Iteration)) %>%
  as.data.frame()
seg.data<-left_join(seg.data, l.length, by="Seg.ID")
hist(seg.data$Seg.len.m) # Histogram of segment lengths in metres
# Convert from SpatialLines to SpatialLinesDataFrame and append line.data information
sldf <- SpatialLinesDataFrame(l, seg.data, match.ID="Seg.ID")
# Save SpatialLinesDataFrame as ESRI shapefile
sldf<-spTransform(sldf, CRS("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
writeOGR(sldf, dsn=getwd(), layer="PRISMM_EffortLines_AllSurveys_Segmented", driver="ESRI Shapefile", overwrite_layer=TRUE)
# Save realized segment info
real_segs <- sldf@data
write.table(real_segs, "PRISMM_SegmentRealizedLengths.txt", sep="\t", quote=FALSE, row.names=FALSE)

### Get midpoints of segments and save as Shapefile
l<-spTransform(l, CRS("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
midpoints<-getSpatialLinesMidPoints(l)
# Computes the weighted mean of the lines elements (mean is called twice); convert from SpatialPoints to SpatialPointsDataFrame including segment ID labels
mpt<-SpatialPointsDataFrame(midpoints, data.frame(Seg.ID=l.length$Seg.ID))
writeOGR(mpt, dsn=getwd(), layer="PRISMM_Segments_Midpoints", driver="ESRI Shapefile", overwrite_layer=TRUE)

### Test plot of a selection of segments and midpoints
plot(sldf[2000:2005,], col=rep(c(1, 2),length.out=length(sldf)), axes = TRUE, las=1)
plot(mpt[2000:2005,], col="blue", pch=20, add=TRUE)
