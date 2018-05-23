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

#----------------------------
# dc setting 
#----------------------------
set compile_slack_driven_buffering true
set_wire_load_mode enclosed
#set compile_delete_unloaded_sequential_cells true
set compile_seqmap_propagate_constants true
set dont_bind_unused_pins_to_logic_constant true
set synlib_model_map_effort high
set hdlin_enable_presto true
set compile_seqmap_synchronous_extraction true
set timing_enable_multiple_clocks_per_reg true
#set_ultra_optimization true
set_fix_multiple_port_nets  -all -buffer_constants
set_cost_priority -delay
set compile_seqmap_identify_shift_registers false
set collection_result_display_limit -1
set power_cg_flatten  true
#set timing_non_unate_clock_compatibility true
set case_analysis_with_logic_constants true
set_leakage_optimization true
set_wire_load_mode enclosed


