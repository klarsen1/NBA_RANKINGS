distance_between <- function(long1, lat1, long2, lat2) {
  R <- 3959
  d <- acos(sin(lat1*pi/180)*sin(lat2*pi/180) + 
              cos(lat1*pi/180)*cos(lat2*pi/180) * cos(long2*pi/180-long1*pi/180)) * R
  if (is.na(d)) {return(0.00)} else {return(d)}
}
