<a name="toc"></a>
- [Parameters](#parameters)
  * [L_rnr_pb](#l_rnr_pb)
  * [L_rnr_per_xpan](#l_rnr_per_xpan)
  * [L_xpan_hdr](#l_xpan_hdr)
  * [L_xpan_rnr](#l_xpan_rnr)
  * [Min_rnr_xpans](#min_rnr_xpans)
  * [N_hdr_per_xpan](#n_hdr_per_xpan)
  * [N_max_hdr_diams](#N_max_hdr_diams)
  * [northsouth_field_sep](#northsouth_field_sep)
  * [offset_xpan_hdr](#offset_xpan_hdr)
  * [V_hdr_cold_max](#v_hdr_cold_max)
  * [V_hdr_cold_min](#v_hdr_cold_min)
  * [V_hdr_hot_max](#v_hdr_hot_max)
  * [V_hdr_hot_min](#v_hdr_hot_min)
- [Outputs](#outputs)
  * [pipe_header_diams](#pipe_header_diams)
  * [pipe_header_expansions](#pipe_header_expansions)
  * [pipe_header_lengths](#pipe_header_lengths)
  * [pipe_header_mdot_dsn](#pipe_header_mdot_dsn)
  * [pipe_header_P_dsn](#pipe_header_P_dsn)
  * [pipe_header_T_dsn](#pipe_header_T_dsn)
  * [pipe_header_vel_dsn](#pipe_header_vel_dsn)
  * [pipe_loop_P_dsn](#pipe_loop_P_dsn)
  * [pipe_loop_T_dsn](#pipe_loop_T_dsn)
  * [pipe_runner_diams](#pipe_runner_diams)
  * [pipe_runner_expansions](#pipe_runner_expansions)
  * [pipe_runner_lengths](#pipe_runner_lengths)
  * [pipe_runner_mdot_dsn](#pipe_runner_mdot_dsn)
  * [pipe_runner_P_dsn](#pipe_runner_P_dsn)
  * [pipe_runner_T_dsn](#pipe_runner_T_dsn)
  * [pipe_runner_vel_dsn](#pipe_runner_vel_dsn)
 
 
<!-- toc -->

## Parameters
### L_rnr_pb
length of runner pipe in meters, for either the hot or cold lines. This length was previously shared with the other identical set of runners for the other half of the solar field, but this is no longer the case. Default = 25 m. [^](#toc)

### L_rnr_per_xpan
the threshold length of straight runner pipe without an expansion loop. Once this length has been reached, an expansion loop is added (without increasing the linear distance). Default = 70 m. [^](#toc)

### L_xpan_hdr
combined length in meters of the two perpendicular segments of a header expansion loop. This is the additional pipe length for each expansion loop. Default = 20 m [^](#toc)

### L_xpan_rnr
combined length in meters of the two perpendicular segments of a runner expansion loop. This is the additional pipe length for each expansion loop. Default = 20 m [^](#toc)

### Min_rnr_xpans
minimum number of expansion loops per single-diameter runner section. Default = 1 [^](#toc)

### N_hdr_per_xpan
number of collector loops per header expansion loops. Default = 2. Value = 1 means that there are expansion loops between every collector loop. [^](#toc)

### N_max_hdr_diams
maximum number of allowed diameters in each of the hot and cold headers. The maximum number of diameters in both the hot and cold headers is 2*N_max_hdr_diams. Default = 10. [^](#toc)

### northsouth_field_sep
north/south separation between subfields, in meters, defined as the shortest distance in-between SCAs in the different fields. Default = 20 m. Value = 0 means SCAs are touching. [^](#toc)

### offset_xpan_hdr
location of the first header expansion loop. Default = 1, which means that the first expansion loop is after the first collector loop closest to the runner. [^](#toc)
				
### V_hdr_cold_max
maximum allowed velocity in the cold header at design conditions. This value can be exceeded if the minimum would also be exceeded, but only if this puts it less out of range. [^](#toc)

### V_hdr_cold_min
minimum allowed velocity in the cold header at design conditions. This value can be exceeded if the maximum would also be exceeded, but only if this puts it less out of range. [^](#toc)

### V_hdr_hot_max
maximum allowed velocity in the hot header at design conditions. This value can be exceeded if the minimum would also be exceeded, but only if this puts it less out of range. [^](#toc)

### V_hdr_hot_min
minimum allowed velocity in the hot header at design conditions. This value can be exceeded if the maximum would also be exceeded, but only if this puts it less out of range. [^](#toc)


## Outputs
### pipe_header_diams
diameters in meters of all of the header sections in the cold and hot headers in one subfield. The first diameter is that before the first set of loops in the cold header and the last diameter is that after the last set of loops in the hot header. [^](#toc)

### pipe_header_expansions
number of expansions or contractions in the given header section [^](#toc)

### pipe_header_lengths
lengths in meters of the all of the header sections, including the added lengths of any expansion loops. The first length is that before the first set of loops in the cold header and last length is that after the last set of loops in the hot header. [^](#toc)

### pipe_header_mdot_dsn
mass flow rate in kg/s of the heat transfer fluid in each header section at design conditions. The first value is in the section before the first set of loops in the cold header and the last value is in the section after the last set of loops in the hot header. The mass flow for the cold header sections is the same as that entering the section, and the mass flow for the hot header sections is the same as that leaving the section. [^](#toc)

### pipe_header_P_dsn
gauge pressure in bar of the heat transfer fluid entering each section of the farthest header at design conditions. The first value is for the section before the first set of loops in the cold header and the last value is for the section after the last set of loops in the hot header. [^](#toc)

### pipe_header_T_dsn
temperature in Celsius of the heat transfer fluid entering each section of the farthest header at design conditions. The first value is for the section before the first set of loops in the cold header and the last value is for the section after the last set of loops in the hot header. [^](#toc)

### pipe_header_vel_dsn
velocity in m/s of the heat transfer fluid in each header section at design conditions. The first value is in the section before the first set of loops in the cold header and the last value is in the section after the last set of loops in the hot header. The velocity for the cold header sections is the same as that entering the section, and the velocity for the hot header sections is the same as that leaving the section. [^](#toc)

### pipe_loop_P_dsn
gauge pressure in bar of the heat transfer fluid entering each node in the farthest loop at design conditions. The values correspond to: [^](#toc)
- 0: &nbsp;&nbsp;&nbsp; the inlet interconnect carrying twice the loop mass flow rate
- 1: &nbsp;&nbsp;&nbsp; the interconnect before the first SCA
- 2: &nbsp;&nbsp;&nbsp; the first SCA
- 3: &nbsp;&nbsp;&nbsp; the interconnect between the first and second SCA
- 4: &nbsp;&nbsp;&nbsp; the second SCA
- ...
- n-3: &nbsp;&nbsp;&nbsp; the last SCA
- n-2: &nbsp;&nbsp;&nbsp; the interconnect after the last SCA
- n-1: &nbsp;&nbsp;&nbsp; the outlet interconnect carrying twice the loop mass flow rate

### pipe_loop_T_dsn
temperature in Celsius of the heat transfer fluid entering each node in the farthest loop at design conditions. The values correspond to: [^](#toc)
- 0: &nbsp;&nbsp;&nbsp; the inlet interconnect carrying twice the loop mass flow rate
- 1: &nbsp;&nbsp;&nbsp; the interconnect before the first SCA
- 2: &nbsp;&nbsp;&nbsp; the first SCA
- 3: &nbsp;&nbsp;&nbsp; the interconnect between the first and second SCA
- 4: &nbsp;&nbsp;&nbsp; the second SCA
- ...
- n-3: &nbsp;&nbsp;&nbsp; the last SCA
- n-2: &nbsp;&nbsp;&nbsp; the interconnect after the last SCA
- n-1: &nbsp;&nbsp;&nbsp; the outlet interconnect carrying twice the loop mass flow rate

### pipe_runner_diams
diameters in meters of the runners listed in L_runner. The first diameter is for the runner that carries half the total mass flow. Example diameters are: [^](#toc)
* 2 field sections = {x1}
* 4 field sections = {x1, x1}
* 6 field sections = {x1, x2}
* 8 field sections = {x1, x1, x3}
* 10 field sections = {x1, x4, x5}

### pipe_runner_expansions
number of expansions or contractions in the given runner section [^](#toc)

### pipe_runner_lengths
lengths in meters of the different diameter runners that extend away from the power block in one direction. L_runner[0] is currently defaulted to 25, which is for the runner piping in and around the power block before it heads out to the field in the main runners. L_runner[0] was previously shared with the other identical set of runners for the other half of the solar field, but this is no longer the case. The runner lengths include expansion loops, except for L_runner[0]. For a given row spacing, SCA length, gap between SCAs, and number of SCA's, example values are: [^](#toc)
* 2 field sections = {L_rnr_pb}
* 4 field sections = {L_rnr_pb, x}
* 6 field sections = {L_rnr_pb, 2x}
* 8 field sections = {L_rnr_pb, x, 2x}
* 10 field sections = {L_rnr_pb, 2x, 2x}

### pipe_runner_mdot_dsn
mass flow rate in kg/s of the heat transfer fluid in each runner section at design conditions. The first value is in the section in and around the power block before it heads out to the field in the main runners. The last value is in the section in and around the power block after it comes back from the field. The mass flow for the cold runner sections is the same as that entering the section, and the mass flow for the hot runner sections is the same as that leaving the section. [^](#toc)

### pipe_runner_P_dsn
pressure in bar of the heat transfer fluid entering each runner section at design conditions. The first value is for the section in and around the power block before it heads out to the field in the main runners. The last value is in the section in and around the power block after it comes back from the field. [^](#toc)

### pipe_runner_T_dsn
temperature in Celsius of the heat transfer fluid entering each runner section at design conditions. The first value is for the section in and around the power block before it heads out to the field in the main runners. The last value is in the section in and around the power block after it comes back from the field. [^](#toc)

### pipe_runner_vel_dsn
velocity in m/s of the heat transfer fluid in each runner section at design conditions. The first value is in the section in and around the power block before it heads out to the field in the main runners. The last value is in the section in and around the power block after it comes back from the field. The velocity for the cold runner sections is the same as that entering the section, and the velocity for the hot runner sections is the same as that leaving the section. [^](#toc)
