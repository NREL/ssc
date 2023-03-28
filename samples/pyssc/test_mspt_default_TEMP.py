import json

import matplotlib.pyplot as plt

import numpy as np

import matplotlib.lines as mlines
import multiprocessing

import sys
import os

import ssc_inout_v2 as ssc_sim


des_dict = json.load(open('MSPT_F7_from_UI__branch.json', 'r'))

print("current processId:", os.getpid());

# Run MSPT simulation
mspt_so_solved_dict = ssc_sim.cmod_mspt_from_dict(des_dict)

is_success = mspt_so_solved_dict["cmod_success"]    
print ('SSC simulation(s) successful = ', is_success)
if (is_success == 1):
    annual_energy = mspt_so_solved_dict["annual_energy"]
    print ('Annual energy (year 1) = ', annual_energy)
    flip_actual_irr = mspt_so_solved_dict["flip_actual_irr"]
    print ('Internal rate of return (IRR) = ', flip_actual_irr)
    print('Rec W_dot = ', mspt_so_solved_dict["W_dot_rec_pump_rec_share_des"])
    print('Tube OD calc = ', mspt_so_solved_dict["od_tube_calc"])
    print('Q TES SAM = ', mspt_so_solved_dict["Q_tes_des"])
    print("W_dot_cycle_pump = ", mspt_so_solved_dict["W_dot_cycle_pump_des"])
    print("q_dot_rec_des = ", mspt_so_solved_dict["q_dot_rec_des"])
    print("solar_mult_calc = ", mspt_so_solved_dict["solar_mult_calc"])
    print("lcoe real = ", mspt_so_solved_dict["lcoe_real"])
    print("total installed cost = ", mspt_so_solved_dict["total_installed_cost"])
else:
    print ('FAIL FAIL FAIL FAIL FAIL')