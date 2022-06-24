# Parameter profiles (for feeding, flying, and roosting)

coFeedingParams <- list(
  # Key parameter values
  MaxSpeedPermited = 120, # in Movebank units (m/s)
  DistThreshold = 50, # max distance for two indivs to be considered interacting
  TimeThreshold = '10 minutes', # for creating timegroups (temporally overlapping)
  MinCoocurForValue = 2, # must co-occur twice in a row to be considered overlapping

  # Other variables
  roostBuffer = 50, # buffer around roosting polygons (m)
  feedingBuffer = 100 # buffer around feeding stations (m)
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

varsToRemove <- c("sensor_type_id","taxon_canonical_name","nick_name","earliest_date_born","sensor","optional",
                  "sensor_type","mw_activity_count","eobs_accelerations_raw","eobs_acceleration_sampling_frequency_per_axis",
                  "eobs_acceleration_axes","argos_valid_location_algorithm","argos_sensor_4","argos_sensor_3","argos_sensor_2",
                  "argos_sensor_1","argos_semi_minor","argos_semi_major","argos_pass_duration","argos_orientation","argos_nopc",
                  "argos_lat1","argos_lat2","1084088","argos_lon1","argos_lon2","argos_nb_mes","argos_nb_mes_120",
                  "eobs_key_bin_checksum","eobs_fix_battery_voltage","eobs_battery_voltage","eobs_status",
                  "eobs_start_timestamp","eobs_type_of_fix","eobs_used_time_to_get_fix","eobs_temperature",
                  "gps_dop","magnetic_field_raw_x","magnetic_field_raw_y","magnetic_field_raw_z","ornitela_transmission_protocol",
                  "tag_voltage","algorithm_marked_outlier","argos_altitude","argos_best_level","argos_lc","argos_iq",
                  "argos_gdop","argos_error_radius","argos_calcul_freq","location_lat.1","location_long.1","timestamps","height_raw",
                  "barometric_pressure","barometric_height","battery_charging_current","eobs_activity","manually_marked_outlier",
                  "eobs_activity_samples", "acceleration_raw_y", "battery_charge_percent", "data_decoding_software","gps_vdop","height_above_ellipsoid",
                  'acceleration_raw_x','acceleration_raw_z',"acceleration_raw_z","eobs_horizontal_accuracy_estimate","eobs_speed_accuracy_estimate")
