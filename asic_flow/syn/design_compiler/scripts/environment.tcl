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

#-------------------------
# Library setting
#-------------------------
set design <TOP_MODULE_NAME>
set search_path "<Provide all library path to be used for synthesis> $search_path"
set target_library    [list]
set link_library      "*"
set synthetic_library [list <List synopsys synthetic library .sldb>]
set target_library    [concat $target_library <List all the target library .db>]

set link_library      [concat $link_library $target_library $synthetic_library ]
#------------------------
# Variables
#-----------------------
set RTL $env(SHAKTI_HOME)/verilog
set DATE [clock format [clock seconds] -format "%b%d-%T"]
set _OUTPUTS_PATH outputs_${DATE}
set _REPORTS_PATH reports_${DATE}
set _LOG_PATH logs_${DATE}
