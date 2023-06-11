/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/ssc/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef __PTES_CYCLE_TEMPLATES_
#define __PTES_CYCLE_TEMPLATES_

class C_ptes_cycle_core
{
public:

    enum E_ptes_state_points
    {
        // Used for both charge and discharge cycles
        PTES_COMP_IN,       // Compressor inlet
        PTES_COMP_OUT,      // Compressor outlet
        PTES_HHX_IN,        // Hot heat exchanger inlet
        PTES_TURB_IN,       // Turbine inlet
        PTES_TURB_OUT,      // Turbine outlet
        PTES_LP_LT_COOL_IN, // Low-pressure low-temperature cooler inlet (could be other cooler locations for different configs)
        PTES_CHX_IN,        // Cold heat exchanger inlet

        END_PTES_STATES
    };

};



#endif
