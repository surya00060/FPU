#Copyright (c) 2013, IIT Madras All rights reserved.
#
#Redistribution and use in source and binary forms, with or without modification, are permitted
#provided that the following conditions are met:
#
#* Redistributions of source code must retain the above copyright notice, this list of conditions
#  and the following disclaimer.  
#* Redistributions in binary form must reproduce the above copyright notice, this list of 
#  conditions and the following disclaimer in the documentation and/or other materials provided 
# with the distribution.  
#* Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or 
#  promote products derived from this software without specific prior written permission.
#
#THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
#OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
#AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
#CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
#DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
#IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
#OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#--------------------------------------------------------------------------------------------------
#
#Author: M Gopinathan
#Email id: gopinathan18@gmail.com
#Details:
#
#--------------------------------------------------------------------------------------------------


#### Template Script for RTL->Gate-Level Flow (generated from RC v11.20-s017_1) 

if {[file exists /proc/cpuinfo]} {
  sh grep "model name" /proc/cpuinfo
  sh grep "cpu MHz"    /proc/cpuinfo
}

puts "Hostname : [info hostname]"

source ../scripts/environment.tcl

set_attribute hdl_unconnected_input_port_value 0 /
set_attr hdl_preserve_unused_registers true /
set_attr hdl_ff_keep_feedback false /
set_attr optimize_constant_0_flops false /
set_attr optimize_constant_1_flops false /
set_attr delete_unloaded_seqs false /
set_attr delete_unloaded_insts false /

set_attribute wireload_mode enclosed /
set_attribute information_level 9 /
set_attribute remove_assigns true



##generates <signal>_reg[<bit_width>] format
set_attribute hdl_array_naming_style %s\[%d\] /

## Turn on TNS, affects global and incr opto
set_attribute tns_opto true /

## Power root attributes
set_attribute lp_power_analysis_effort high /
set_attribute lp_power_unit mW /
set_attribute lp_toggle_rate_unit /ns /

## Dont use cells
source ../scripts/fab_specific.tcl

####################################################################
## Load Design
####################################################################
read_hdl -v2001 ${rtl_list}

elaborate $DESIGN
puts "Runtime & Memory after 'read_hdl'"
timestat Elaboration




check_design -unresolved
###################################################################
## Constraints Setup
####################################################################
read_sdc constraints.sdc
puts "The number of exceptions is [llength [find /designs/$DESIGN -exception *]]"

#set_attribute force_wireload G30K "/designs/$DESIGN"

if {![file exists ../${_LOG_PATH}]} {
  file mkdir ../${_LOG_PATH}
  puts "Creating directory ${_LOG_PATH}"
}

if {![file exists ../${_OUTPUTS_PATH}]} {
  file mkdir ../${_OUTPUTS_PATH}
  puts "Creating directory ${_OUTPUTS_PATH}"
}

if {![file exists ../${_REPORTS_PATH}]} {
  file mkdir ../${_REPORTS_PATH}
  puts "Creating directory ${_REPORTS_PATH}"
}

###################################################################################
## Define cost groups (clock-clock, clock-output, input-clock, input-output)
###################################################################################
## Uncomment to remove already existing costgroups before creating new ones.
## rm [find /designs/* -cost_group *]

if {[llength [all::all_seqs]] > 0} { 
  define_cost_group -name I2C -design $DESIGN
  define_cost_group -name C2O -design $DESIGN
  define_cost_group -name C2C -design $DESIGN
  path_group -from [all::all_seqs] -to [all::all_seqs] -group C2C -name C2C
  path_group -from [all::all_seqs] -to [all::all_outs] -group C2O -name C2O
  path_group -from [all::all_inps]  -to [all::all_seqs] -group I2C -name I2C
}

define_cost_group -name I2O -design $DESIGN
path_group -from [all::all_inps]  -to [all::all_outs] -group I2O -name I2O
foreach cg [find / -cost_group *] {
   report timing -num_paths 1000 -worst 1 -cost_group [list $cg] >> ../$_REPORTS_PATH/${DESIGN}_pretim.rpt
}
check_design -multidriven
####################################################################################################
## Synthesizing to generic 
####################################################################################################
synthesize -to_generic -eff $SYN_EFF
puts "Runtime & Memory after 'synthesize -to_generic'"
timestat GENERIC
report datapath > ../$_REPORTS_PATH/${DESIGN}_datapath_generic.rpt
generate_reports -outdir ../$_REPORTS_PATH -tag generic
summary_table -outdir ../$_REPORTS_PATH

####################################################################################################
## Synthesizing to gates
####################################################################################################

## Add '-auto_identify_shift_registers' to 'synthesize -to_map' to automatically 
## identify functional shift register segments.
synthesize -to_mapped -eff $MAP_EFF -no_incr
puts "Runtime & Memory after 'synthesize -to_map -no_incr'"
timestat MAPPED
report datapath > ../$_REPORTS_PATH/${DESIGN}_datapath_map.rpt

foreach cg [find / -cost_group *] {
   report timing -num_paths 1000 -worst 1 -cost_group [list $cg] > ../$_REPORTS_PATH/${DESIGN}_[basename $cg]_post_map.rpt
}
generate_reports -outdir ../$_REPORTS_PATH -tag map
summary_table -outdir ../$_REPORTS_PATH
report timing > ../$_REPORTS_PATH/${DESIGN}_timing.rpt


##Intermediate netlist for LEC verification..
write_hdl -lec > ../${_OUTPUTS_PATH}/${DESIGN}_intermediate.v
write_do_lec -revised_design ../${_OUTPUTS_PATH}/${DESIGN}_intermediate.v -logfile ../${_LOG_PATH}/rtl2intermediate.lec.log > ../${_OUTPUTS_PATH}/rtl2intermediate.lec.do

#######################################################################################################
## Incremental Synthesis
#######################################################################################################
synthesize -to_mapped -eff $MAP_EFF -incr   
generate_reports -outdir ../$_REPORTS_PATH -tag incremental
summary_table -outdir ../$_REPORTS_PATH

puts "Runtime & Memory after incremental synthesis"
timestat INCREMENTAL

foreach cg [find / -cost_group -null_ok *] {
  report timing -num_paths 1000 -worst 1 -cost_group [list $cg] > ../$_REPORTS_PATH/${DESIGN}_[basename $cg]_post_incr.rpt
}

report timing > ../$_REPORTS_PATH/${DESIGN}_timing_incr.rpt

synthesize -to_mapped -eff low -incr
puts "Runtime & Memory after incremental synthesis"
timestat INCREMENTAL_POST_SCAN_CHAINS


report clock_gating > ../$_REPORTS_PATH/${DESIGN}_clockgating.rpt
report power -depth 0 > ../$_REPORTS_PATH/${DESIGN}_power.rpt
report gates -power > ../$_REPORTS_PATH/${DESIGN}_gates_power.rpt

report qor > ../$_REPORTS_PATH/${DESIGN}_qor.rpt
report area > ../$_REPORTS_PATH/${DESIGN}_area.rpt
report datapath > ../$_REPORTS_PATH/${DESIGN}_datapath_incr.rpt
report messages > ../$_REPORTS_PATH/${DESIGN}_messages.rpt
write_design -basename ../${_OUTPUTS_PATH}/${DESIGN}_m
write_hdl  > ../${_OUTPUTS_PATH}/${DESIGN}_m.v
write_script > ../${_OUTPUTS_PATH}/${DESIGN}_m.script
write_sdc > ../${_OUTPUTS_PATH}/${DESIGN}_m.sdc
report timing > timing.txt
report area > area.txt
report power > power.txt
report qor > qor.txt


puts "Final Runtime & Memory."
timestat FINAL
puts "============================"
puts "Synthesis Finished ........."
puts "============================"
date
file copy [get_attr stdout_log /] ../${_LOG_PATH}/.
##quit
