# Parameter profiles (for feeding, flying, and roosting)

coFeedingParams <- list(
  # Key parameter values
  MaxSpeedPermited = 120, # in Movebank units (m/s)
  DistThreshold = 50, # max distance for two indivs to be considered interacting
  TimeThreshold = '10 minutes', # for creating timegroups (temporally overlapping)
  MinCoocurForValue = 2, # must co-occur twice in a row to be considered overlapping

  # Other variables
  roostbuffer = 50, # buffer around roosting polygons (m)
  feedBuff = 100 # buffer around feeding stations (m)
)

coFlightParams <- list(
  # Key parameter values
  MaxSpeedPermited = NA, # in Movebank units (m/s)
  DistThreshold = NA, # max distance for two indivs to be considered interacting
  TimeThreshold = '10 minutes', # for creating timegroups (temporally overlapping)
  MinCoocurForValue = NA, # must co-occur twice in a row to be considered overlapping

  # Other variables
  roostbuffer = NA, # buffer around roosting polygons (m)
  feedBuff = NA # buffer around feeding stations (m)
)

coRoostingParams <- list(
  # Key parameter values
  MaxSpeedPermited = NA, # in Movebank units (m/s)
  DistThreshold = NA, # max distance for two indivs to be considered interacting
  TimeThreshold = '10 minutes', # for creating timegroups (temporally overlapping)
  MinCoocurForValue = NA, # must co-occur twice in a row to be considered overlapping

  # Other variables
  roostbuffer = NA, # buffer around roosting polygons (m)
  feedBuff = NA # buffer around feeding stations (m)
)
