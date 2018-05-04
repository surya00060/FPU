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

set unq_input_list [all_inputs]
foreach_in_collection unq_clock_element [all_clocks] {
     set unq_clock_name [get_port  $unq_clock_element]
     set unq_input_list [remove_from_collection $unq_input_list $unq_clock_name]
}
group_path -name f2f -from [all_registers]  -to [all_registers] -critical_range 0.7
group_path -name i2f -from $unq_input_list -to [all_registers] -critical_range 0.7
group_path -name f2o -from [all_registers]  -to [all_outputs]   -critical_range 0.7
group_path -name i2o -from $unq_input_list -to [all_outputs]   -critical_range 0.7
