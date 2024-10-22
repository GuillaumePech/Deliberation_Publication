# -*- coding: utf-8 -*-
"""
Created on Mon Apr 25 11:19:28 2022

@author: mfbpe
"""
############################################################
###############    Triggers code      ######################
############################################################
# conditions : 10 20 30 - Arbitrary / Midly Deliberate / Fully Deliberate
# handpress  : 1 left 2 right + conditions (e.g left arbitrary = 11)
# Stim       : 0 Stim appear + conditions
# Beep       : 3 - 4 - 5 - 200ms / 500ms / 800 ms  + conditions
# Scale Estim: 6 - 7 apparation / click + condition


import os
from psychopy import core, visual, event, gui
import csv, random, time
import winsound
import serial
import os.path
import tkinter as tk
import urllib.request as urllib2
import json
from threading import Thread
import numpy as np

root = tk.Tk()

os.chdir('C:/Users/mfbpe/OneDrive/Cognitive_Science/experiments/deliberation/data_bids/code/task/')
save_path = 'C:/Users/mfbpe/OneDrive/Cognitive_Science/experiments/deliberation/data_bids/code/task/'

# os.chdir('C:/Users/Admin/Desktop/Pech_Volition/Deliberation_Pilot')
#os.chdir('C:/Users/Admin/Desktop/NGO_Game/VShock/')

screen_width = root.winfo_screenwidth()
screen_height = root.winfo_screenheight()

# screen_width = 1920
# screen_height = 1080

def scan():
    available = []
    for i in range(256) : 
        try : 
            s= serial.Serial("COM"+str(i))
            available.append((s.portstr))
            s.close()
        except serial.SerialException :
            pass
    return available[0]

# https://www.ascii-code.com/ to find the corresponding b'' decimal trigger its decimal to hexadecimal

# port = serial.Serial(str(scan()), baudrate = 115200)

def send_trigger(trigger):
    try : 
         port.write(trigger)
    except:
        pass

def geturl(url, sensors): #url is the http command to call the neulog sensor, sensors is to select the left (=1) or right (=2) sensor
    global value_sensors_1, value_sensors_2
    fd = urllib2.urlopen(url)
    content = fd.read()
    fd.close()
    try:
        d = json.loads(content)
        if sensors ==1:
            value_sensors_1.append((float(d['GetSensorValue'][0])))
        if sensors ==2:
            value_sensors_2.append((float(d['GetSensorValue'][0])))
    except:
        pass
 

def handpress_without_trigger():
    global keypress
    while True:
        thread = Thread(target = geturl, args = (url + 'GetSensorValue:[HandDynamometer],[1]',1)) #start the record of the left sensor
        thread.start()
        thread = Thread(target = geturl, args = (url + 'GetSensorValue:[HandDynamometer],[2]',2))#start the record of the right sensor
        thread.start()
        time.sleep(0.1)

        if (max(value_sensors_1))>1:
            keypress ='left'
            break 
        if (max(value_sensors_2))>1:
            keypress ='right'
            break

def handpress_left_calibration():
    global keypress, rect, win
    start = time.time()
    end = time.time()
    while end-start <6:
        thread = Thread(target = geturl, args = (url + 'GetSensorValue:[HandDynamometer],[1]',1)) #start the record of the left sensor
        thread.start()
        time.sleep(0.1)
        
        if end-start <1.7:
            rect.setHeight(0.4)
            rect.setFillColor("#FF0000")
        elif end-start >1.7 and end-start<2 :
            rect.setFillColor("#00CC00")
        else :
            
            rect.setFillColor("#00CC00")
            rect.setHeight(value_sensors_1[-1]/30)
        rect.draw()
        win.flip()
        
        end = time.time()
    

def handpress_right__calibration():
    global keypress, rect, win
    start = time.time()
    end = time.time()
    while end-start <6:
        thread = Thread(target = geturl, args = (url + 'GetSensorValue:[HandDynamometer],[2]',2)) #start the record of the left sensor
        thread.start()
        time.sleep(0.1)
        
        if end-start <1.7:
            rect.setHeight(0.4)
            rect.setFillColor("#FF0000")
        elif end-start >1.7 and end-start<2 :
            rect.setFillColor("#00CC00")
        else :
            
            rect.setFillColor("#00CC00")
            rect.setHeight(value_sensors_2[-1]/30)
        rect.draw()
        win.flip()
        end = time.time()
    
def handpress():
    global keypress
    while True:
     thread = Thread(target = geturl, args = (url + 'GetSensorValue:[HandDynamometer],[1]',1)) #start the record of the left sensor
     thread.start()
     thread = Thread(target = geturl, args = (url + 'GetSensorValue:[HandDynamometer],[2]',2))#start the record of the right sensor
     thread.start()
     time.sleep(0.1)

     if (max(value_sensors_1))>1:
         keypress ='left'
         send_trigger(trigger_handpress[condition][0])
         break 
     if (max(value_sensors_2))>1:
         keypress ='right'
         send_trigger(trigger_handpress[condition][1])
         break 

  
# to quit the experiment
def save_and_quit() : 
    global datafile, port, datafile_qstr,datafile_qstion, win, core, event, exp_info, condition, RT_asw, Tps_esti, Tps_real, duo_presented, answanswers_qstr,percentage_block,value_sensors_answ,hand_sensors_answ,writer

    datafile.close()
    datafile_qstr.close()
    datafile_qstion.close()
    win.close()
    port.close()
    core.quit()
    event.clearEvents()
    
url = 'http://localhost:22004/NeuLogAPI?' #to communicate to the handgrip sensor via Json HTTP
geturl(url + 'GetSeverStatus',0) #Start the communication with the neulog software
geturl(url + 'ResetSensor:[HandDynamometer],[1]',0) # Reset the left sensor value 
geturl(url + 'ResetSensor:[HandDynamometer],[2]',0) # Reset the right sensor value


thread = Thread(target = geturl, args = (url + 'GetSensorValue:[HandDynamometer],[1]',1)) #start the record of the left sensor
thread.start()


##############################################################################
####################                  ##########################################################
#################### NUMBERS OF TRIAL #######################################
####################                  ##########################################################
##############################################################################

nb_trials_training = 0 #nb of trials for the training
nb_trials_more = 1 #add a nb of trials after the first set of trials if participant need more training

nb_trials = 1 # Nb trials Arbi - MDelib - Fdelib

conditions= []
for i in range(1): # sets of the 3 conditions
    index_cond=[0,1,2]  #Index condition  0 Arbitrary - 1 Midly deliberate - 2 Fully deliberate
    random.shuffle(index_cond)
    conditions.extend(index_cond)
 
randomBeep = [] ;

for i in range(9): #6 blocks (2 arbitrary - 2 midly - 2 full)
    shuffled_beep =  [0,0.300,0.600]*8 #24 trials per blocks, 8 of each per block
    random.shuffle(shuffled_beep)
    randomBeep.extend(shuffled_beep)
    
##############################################################################
####################                  ##########################################################
####################    VARIABLES     #######################################
####################                  ##########################################################
##############################################################################

# Variables
asw_baloon=[] #baloon chosen
asw_token=[] #token chosen
answer_final = list(range(3)) #Three last questions about the strategy 
value_baloon_selected=[] #baloon chosen
value_token_selected=[] #token chosen
value_baloon_not_selected=[] #baloon chosen
value_token_not_selected=[] #token chosen
real_value_baloon_selected=[] #baloon chosen
real_value_token_selected=[] #token chosen
real_value_baloon_not_selected=[] #baloon chosen
real_value_token_not_selected=[] #token chosen
qstr = list(range(11)) #questionnaire end of blocks
RT_asw =[] 
Tps_esti =[] #Estimated Time Intentional Binding
Tps_real =[] #Real Time Beep
answers_qstr = [] #record the value for the questionaire
percentage_block = [] #Percentage of the maximum score reach each block
final_reward = 0 #final value of the seleted stim
max_reward = 0 #max value possibly achieve during the task
min_reward = 0 #min value possibly achieve during the task
stock_reward  = 0 #to display the neuro stock during the study
stock_reward_bis = 0 #To have a level similar during the block and change between block
max_reward_block = 0 #to count the max reward on each block and display the amount reach at the end of the block
min_reward_block = 0
final_reward_block = 0
value_sensors_1 = [0] #create a variable for the value of the sensor left
value_sensors_2 = [0] #create a variable for the value of the sensor right
value_sensors_answ = [] #stock the value of the squeezing strenght 
hand_sensors_answ = [] #stock the side of the hand (left/right)


## create variables and stim order - value
value_20 = [8 ,9 ,10,11, 12, 13, 14, 15];
value_30 = [3, 4, 5, 6, 7 ,8 ,9 ,10, 11];
value_40 = [1,2, 3, 4, 5, 6, 7 ,8 ,9]; 
index_20 = [0,1,2,3,4,5,6,7]
index_30 = [8,9,10,11,12,13,14,15,16]
index_40 = [17,18,19,20,21,22,23,24,25]
index_all = index_20,index_30,index_40


value_stim = value_20+value_30+value_40;
value_stim_token = value_stim


#combinaison (cbs) list shuffled with an index 
cbs_sort = [] 
cbs_index_sort = []
cbs_value_sort = []
cbs_shuffle = [] 
cbs_value_shuffle = []
cbs_index_shuffle = []

a= 0
b=10 

value_20_cbs =  [((val+1)/17)*20 for val in value_20]
value_30_cbs =  [((val+1)/17)*30 for val in value_30]
value_40_cbs =  [((val+1)/17)*40 for val in value_40]


# to create 3 list of stimuli for the 3 category (20neuro - 30neuro - 40neuro) (total 9)
# 1 is the filling value - 1 is the real value of the stim - 1 is the index of the value
# As the filling value can be similar in the 30 neuro and 40 neuro category for instance
# the index value permit to not overlap and know which one is presented
j=0
k=0
for create_list in range(81):  
    if create_list <72:
        xy = [value_20[j],value_30[k]] #to create list of value filling
        xy1 = [value_20_cbs[j],value_30_cbs[k]] # to create a list of value reward corresponding
        xy2 = [index_20[j],index_30[k]]#to create a list of index value corresponding
        cbs_sort.append(xy)
        cbs_value_sort.append(xy1)
        cbs_index_sort.append(xy2)
        
        xy = [value_20[j],value_40[k]]
        xy1 = [value_20_cbs[j],value_40_cbs[k]]
        xy2 = [index_20[j],index_40[k]]
        cbs_sort.append(xy)
        cbs_value_sort.append(xy1)    
        cbs_index_sort.append(xy2)
        
    xy = [value_30[j],value_40[k]]
    xy1 = [value_30_cbs[j],value_40_cbs[k]]
    xy2 = [index_30[j],index_40[k]]
    cbs_sort.append(xy)
    cbs_value_sort.append(xy1)    
    cbs_index_sort.append(xy2)
   
    k+=1
    if k == 9:
        j+=1
        k=0

#create a list to shuffle in the same way the three  list
random_order = list(range(225)) # create a list from 0 to 225 (number of different combinaisons possible)
random.shuffle(random_order)

for create_shuffle_list in random_order:
    cbs_shuffle.append(cbs_sort[create_shuffle_list])
    cbs_value_shuffle.append(cbs_value_sort[create_shuffle_list])
    cbs_index_shuffle.append(cbs_index_sort[create_shuffle_list])

#reverse randomly presentation left-right of the stimulus 
for reverse_position in range(len(cbs_shuffle)):
    reverse_or_not =random.randint(0,1) #create a random value 0 or 1 to reverse or not
    if reverse_or_not ==1 :
            cbs_shuffle[reverse_position] = list(reversed(cbs_shuffle[reverse_position]))
            cbs_value_shuffle[reverse_position] = list(reversed(cbs_value_shuffle[reverse_position]))
            cbs_index_shuffle[reverse_position] = list(reversed(cbs_index_shuffle[reverse_position]))


cbs_shuffle_token = list(reversed(cbs_shuffle)) #reverse combinaison for token list regarding the baloon combinaison
cbs_value_shuffle_token = list(reversed(cbs_value_shuffle))  
cbs_index_shuffle_token = list(reversed(cbs_index_shuffle))  


#trigger_handpress = [[11, 12],[ 21, 22],[ 31, 32]]
#trigger_stim = [[10],[20],[30]]
#trigger_beep = [[13, 14, 15],[ 23, 24,25],[ 33, 34,35]]
#trigger_scale = [[16, 17],[ 26, 27],[ 36, 37]]
trigger_handpress = [[b'\x0B', b'\x0C'],[b'\x15', b'\x16'],[b'\x1F', b'\x20']]
trigger_stim = [[b'\x0A'],[b'\x14'],[b'\x1E']]
trigger_beep = [[b'\x0D', b'\x0E', b'\x0F'],[b'\x17', b'\x18',b'\x19'],[b'\x21', b'\x22',b'\x23']]
trigger_scale = [[b'\x10', b'\x11'],[ b'\x1A', b'\x1B'],[b'\x24', b'\x25']]


# create list of value gauge for arbitrary - midly deliberate and fully deliberate.
# first create a list of two, generate a random number between 0 and 1
# attribute one of the two list
value_gauge = [[[[0,0,0],[0,0,0]],[[40,30,20],[0,0,0]],[[40,30,20],[40,30,20]]],[[[0,0,0],[0,0,0]],[[0,0,0],[40,30,20]],[[40,30,20],[40,30,20]]]]
value_gauge_real = [[[[0,0,0],[0,0,0]],[[40,30,20],[0,0,0]],[[40,30,20],[40,30,20]]],[[[0,0,0],[0,0,0]],[[0,0,0],[40,30,20]],[[40,30,20],[40,30,20]]]]
value = [20,30,40]
nb_rdm = random.randint(0,1) 
value_gauge = value_gauge[nb_rdm]
value_gauge_real = value_gauge_real[nb_rdm]
name_condition = ['0 système activté', '1 système activé', '2 systèmes activés' ]
tag_condition = ['Arbitrary', 'Midly_Deliberate', 'Fully_Deliberate']
i_random = [[0,1,2],[3,4,5]] # if need to randomize the color - value

#-------- Infos sujet ------------------------------------------------------------------------------

okfile = 0

while okfile != 1 : 
    exp_info = {"1-Participant":"","2-Age":"", "3-Version":""} # open a box and ask information partcipant and age and store in a dictionnary {}
    dlg = gui.DlgFromDict(dictionary = exp_info)
    if dlg.OK == False:
        core.quit()


    dataname = [save_path+"Pilot_Deliberation_Participant_"+exp_info["1-Participant"]+".csv"]
    dataname_qstr = [save_path+"Pilot_Deliberation_Questionaire_Participant_"+exp_info["1-Participant"]+".csv"]
    dataname_qstion = [save_path+"Pilot_Deliberation_Questions_Participant_"+exp_info["1-Participant"]+".csv"]

    if os.path.isfile(dataname[0]):
        myDlg = gui.Dlg(title="The file already exist")
        myDlg.addField("Do you want to erase the existing file?", choices=["Yes","No"])
        Part_asw = myDlg.show() ; # show dialog and wait for OK or Cancel
        if myDlg.OK ==False:  # or if ok_data is not None
            core.quit()
        
        if Part_asw == ["Yes"] : # show dialog and wait for OK or Cancel
            okfile = 1
        elif Part_asw == ["No"]: 
            okfile = 0
    else:
        okfile = 1

#-------- Création du CSV --------------------------------------------------------------------------

datafile=open(dataname[0],"w", newline="")
writer=csv.writer(datafile, delimiter=";");
writer.writerow(["Participant", "Age", "Block", "Condition", "Trial", "RT_Answer", "Tps_Esti",  "Tps_Real", "Strength_Squeeze",  "Hand_0left_1right", "Duo_Baloon_Presented", "Duo_Token_Presented","Value_Baloon_Selected","Value_Token_Selected","Value_Baloon_Not_Selected","Value_Token_Not_Selected","real_value_baloon_selected", "real_value_token_selected","real_value_baloon_not_selected", "real_value_token_not_selected"])

datafile_qstr=open(dataname_qstr[0],"w", newline="")
writer_qstr=csv.writer(datafile_qstr, delimiter=";");
writer_qstr.writerow(["Participant", "Age", "Block", "Condition", "Percentage_Perf","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11"])

datafile_qstion=open(dataname_qstion[0],"w", newline="")
writer_qstion=csv.writer(datafile_qstion, delimiter=";");
writer_qstion.writerow(["Participant", "Age", "Version", "Strat_arbi", "Strat_mdeli", "Strat_fdeli"])
 
  
#-------- Define stim and open the window------------------------------------------------------------------------
# win = visual.Window([screen_width, screen_height], color=[-1,-1,-1], fullscr=True) #create a windows of 800 800 if fullscr off / individu -1 black 1 white
win = visual.Window([1440, 900], color=[-1,-1,-1], fullscr=False) #create a windows of 800 800 if fullscr off / individu -1 black 1 white

#stim text
stim = visual.TextStim(win, font="Arial", wrapWidth=(1.8))
stim.setHeight(0.15)
stim.setColor("white")
stim.setText("Calibrage Handgrips.  Appuyer sur 'entrée' après que le pointeur de souris ai disparu.")
stim.draw()
win.flip()

#stim2 and stim3 for the +1 when bucket are filled
stim2 = visual.TextStim(win, font="Arial", wrapWidth=(1.8), pos=(1,1)) #stim 2 is for baloon
stim2.setHeight(0.08)
stim2.setColor("black")
stim2.setText("+1")
stim3 = visual.TextStim(win, font="Arial", wrapWidth=(1.8),pos=(1,1)) #stim 3 is for token
stim3.setHeight(0.08)
stim3.setColor("black")
stim3.setText("+1")
#task 
task = [0,1,2]

#circle answer 
circle = visual.Circle(win, radius=(0.22,0.63), edges=100, units='', 
                       lineWidth=4.5, lineColor='white',
                       lineColorSpace='white', size = 1.5)
#rectangle around the stock reward
            
rect = visual.Rect(
    win=win,
    units="norm",
    pos=(0,0),
    width=0.3,
    height=0.1,
    colorSpace='rgb',
    fillColor="#FF0000",
    lineColor="#606060"
)


#neuro symbol
neuro = visual.ImageStim(win, 'Neuro.PNG', units = "norm", size=(0.06,0.12), pos = (-0.78,-0.05))  

# Line for separate screen in two 
line = visual.Line(win,start= (-1, 0), end=(1, 0), pos = (0,-0.05), units=None, lineWidth=1.5, lineColor='white')

#questionnaires slider 
slider2 = visual.Slider(win,units= 'norm',font="Arial", size = (1.5, 0.06), pos=(0,-0.1), 
                        ticks=(0, 100), labels =["0", "100"], granularity =0.0,color='White', fillColor='Orange',
                        borderColor='White', colorSpace='rgb',labelHeight=0.15)
slider2.getMouseResponses() #ask to have mouse responses for the slider
questionnaire_slider = visual.TextStim(win, color = [1,1,1], units = 'norm',
        font="Arial", height = 0.13, pos=(0,0.2), wrapWidth=(1.5))

questions_SOAS = ["Mes sélections de lot étaient planifiées",
                  "Je sentais que j'étais la cause des sélections de lot",
                  "Mes sélections de lot étaient sous mon contrôle",
                  "J'ai été surpris par mes sélections de lot",
                  "Je sentais que mes sélections de lot étaient inévitables",
                  "Je sentais que mes sélections de lot étaients voulues par moi-même",
                "Mes sélections de lot étaient soumises à ma propore volonté, sans contraintes",
                   "Mes sélection de lot étaient involontaires",
                  "Mes sélections de lot était sans effort",
                  "Je sentais que mes sélections de lot étaient générées par moi-même",
                 "Mes sélections de lot n'étaient pas intentionnelles"]

# Estimation interval stimuli

slider = visual.Slider(win,units= 'norm', size = (1.5, 0.065), pos=(0,-0.23),  ticks=(0, 200,400,600,800, 1000), labels =[0,200,400,600,800, '1000 ms'], granularity =0.0,color='White', fillColor='Yellow', borderColor='White', colorSpace='rgb',)
slider.getMouseResponses() #ask to have mouse responses for the slider

Beep = visual.ImageStim(win, "Beep.PNG", units = "norm", pos=(0.75, -0.05), size = (0.13))
Keypress = visual.ImageStim(win, "Keypress.PNG", units = "norm", pos=(-0.72, -0.05), size = (0.18))
interval_msg = visual.TextStim(win, text = "Estimez le temps écoulé entre l'appui sur le handgrip et le bip sonore", 
        color = [1,1,1], units = 'norm', height = 0.1, pos=(0,0.5), wrapWidth=(1.8))

##########################################################
##                                    ########################################################
##   Baloon, Token & Gauges stimuli   ############################################################
##                                    ########################################################
##########################################################
# Gauges value, index and pos
gauge_actual = [0, 18, 36, 54, 72, 90] #index of the 6 gauges
gauge_pos = [(-0.25,0.5), (0.25,0.5), (0.75,0.5), (-0.25,-0.5), (0.25,-0.5), (0.75,-0.5)]

value_pos = [(-0.29,0.38), (0.21,0.38), (0.71,0.38), (-0.29,-0.62), (0.21,-0.62), (0.71,-0.62)]
neutral_baloon = visual.ImageStim(win, 'Baloon_Neutral.PNG',
                                  units = "norm", size=(0.25,0.85), pos=(-0.75,0.5))   
token = visual.ImageStim(win, 'Token.PNG',
                                  units = "norm", size=(0.3,0.4), pos=(-0.75,-0.55))   
arrow = visual.ImageStim(win, 'arrow.PNG',
                                  units = "norm", size=(0.35,0.4), pos=(-0.75,-0.55))   



All_Baloon=[]

version = int(exp_info["3-Version"])

if version == 1:
    for i in range(45):  #add the value stim in one long list
        if i<15:
            All_Baloon.append(visual.ImageStim(win, 'Red_Baloon'+str(i+1)+'.PNG', units = "norm"))
        elif i>14 and i <30:
            All_Baloon.append(visual.ImageStim(win, 'Green_Baloon'+str(i-14+1)+'.PNG', units = "norm"))
        elif i>29:
            All_Baloon.append(visual.ImageStim(win, 'Blue_Baloon'+str(i-28+1)+'.PNG', units = "norm"))
    
    gauge_names = ["Gauge_Red","Gauge_Green","Gauge_Blue", "Gauge_Pink", "Gauge_Yellow", "Gauge_Cyan"]
    gauge_color_token =  15*['pink']+ 15*['yellow'] +15*['cyan']
elif version == 2:
    for i in range(45):  #add the value stim in one long list
        if i<15:
            All_Baloon.append(visual.ImageStim(win, 'Blue_Baloon'+str(i+1)+'.PNG', units = "norm"))
        elif i>14 and i <30:
            All_Baloon.append(visual.ImageStim(win, 'Red_Baloon'+str(i-14+1)+'.PNG', units = "norm"))
        elif i>29:
            All_Baloon.append(visual.ImageStim(win, 'Green_Baloon'+str(i-28+1)+'.PNG', units = "norm"))

    gauge_names = ["Gauge_Blue","Gauge_Red","Gauge_Green", "Gauge_Cyan", "Gauge_Pink", "Gauge_Yellow"]
    gauge_color_token =  15*['cyan']+ 15*['pink'] +15*['yellow']  

#All_baloon 0-13 red, 14-27 green, 28-41 blue
gauge_stim=[]
for i in range(6):
    for j in range(18):    
        gauge_stim.append(visual.ImageStim(win, gauge_names[i]+str(j)+'.PNG', units = "norm", size=(0.3,0.9)))   



#-------- Start of the experiment ------------------------------------------------------------------------
cursor = event.Mouse(visible=0, newPos=(0,-0.3)) #to manage the cursor visibility and position
event.waitKeys(keyList = 'return') #after the stim début de l'experience, wait for return to start
# cursor.setVisible(0)


while True :
        
    stim = visual.TextStim(win, font="Arial", wrapWidth=(1.8))
    stim.setHeight(0.15)
    stim.setColor("white")
    stim.setText("Calibration main gauche, appuyer sur la touche 'entrée' pour commencer, attendez que le rectangle soit vert avant de presser le handgrip. Vous avez 6 secondes pour le serrer aussi fort que possible avec votre main.")
    stim.draw()
    win.flip()
    event.waitKeys(keyList = 'return') #after the stim début de l'experience, wait for return to start
        
    thread.join()
    value_sensors_1 = [0]
    value_sensors_2 = [0]
    handpress_left_10sec()
    
    thread.join()
    squeeze_max_left =max(value_sensors_1)

    stim = visual.TextStim(win, font="Arial", wrapWidth=(1.8))
    stim.setHeight(0.15)
    stim.setColor("white")
    stim.setText("Voulez-vous recommencer (Espace) ou continuer (Entrée)")
    stim.draw()
    win.flip()
    key = event.waitKeys(keyList = ['return', 'space']) #after the stim début de l'experience, wait for return to start
    if key[0] == 'return':
        break

while True :
        
    stim = visual.TextStim(win, font="Arial", wrapWidth=(1.8))
    stim.setHeight(0.15)
    stim.setColor("white")
    stim.setText("Calibration main droite, appuyer sur la touche 'entrée' pour commencer, attendez que le rectangle soit vert avant de presser le handgrip. Vous avez 6 secondes pour le serrer aussi fort que possible avec votre main.")
    stim.draw()
    win.flip()
    event.waitKeys(keyList = 'return') #after the stim début de l'experience, wait for return to start
         
    thread.join()
    value_sensors_1 = [0]
    value_sensors_2 = [0]
    handpress_right_10sec()
    
    thread.join()
    squeeze_max_right =max(value_sensors_2)
    
    stim = visual.TextStim(win, font="Arial", wrapWidth=(1.8))
    stim.setHeight(0.15)
    stim.setColor("white")
    stim.setText("Voulez-vous recommencer (Espace) ou continuer (Entrée)")
    stim.draw()
    win.flip()
    key = event.waitKeys(keyList = ['return', 'space']) #after the stim début de l'experience, wait for return to start
    if key[0] == 'return':
        break
   


stim = visual.TextStim(win, font="Arial", wrapWidth=(1.8))
stim.setHeight(0.15)
stim.setColor("white")
stim.setText("Début entraînement. Appuyer sur 'entrée'.")
stim.draw()
win.flip()
event.waitKeys(keyList = 'return') #after the stim début de l'experience, wait for return to start

############################################################################################
########################## TRAINING ###########################################################
############################################################################################
trial = 0 #iteration trials
value_gauge_training = [[40,30,20],[40,30,20]]
baloon_color_corresponding_value = index_all
token_color_corresponding_value = index_all

while trial != nb_trials_training: #number of trial
        ###################### Draw the 6 gauges level ##################################################
        #draw 6 gauges
        if trial == 0 :
            gauge_actual = [0, 18, 36, 54, 72, 90]
        stim.setHeight(0.15) #txt for the value inside
        stim.setColor('black')
                
        for i in range(6):
            if i < 3: # to display the value on the system up and down
                stim.setText(value_gauge_training[0][i])
                gauge_stim[gauge_actual[i_random[0][i]]].setPos(gauge_pos[i])
                gauge_stim[gauge_actual[i_random[0][i]]].draw() 
            else : 
                stim.setText(value_gauge_training[1][i-3])
                gauge_stim[gauge_actual[i_random[1][i-3]]].setPos(gauge_pos[i])
                gauge_stim[gauge_actual[i_random[1][i-3]]].draw()                 
                
            stim.setPos(value_pos[i])
            stim.draw()
        
        stim2.draw()  #draw the +1 on a baloon
        stim3.draw() #draw the +1 on a token         
        neutral_baloon.draw()
        token.setPos((-0.75,-0.55))
        token.setSize((0.3,0.4))
        token.draw()
        line.draw()
        stim.setHeight(0.15) #txt for the value stock reward
        win.flip()
        stim2.setColor("black") #make the +1 invisible except if a new gauge is filled
        stim3.setColor("black")
        
        cursor.setPos((-1,-1))
       
        thread.join()
        time.sleep(0.5)
        value_sensors_1 = [0]
        value_sensors_2 = [0]
        handpress_without_trigger()
                
        key = event.getKeys(keyList = 'escape')
        if len(key) > 0:
                save_and_quit()

        ###################### THE FIXATION CROSS ##################################################
        stim.setText('+') #fixation cross
        stim.setColor('white')
        stim.setHeight(0.1)
        stim.setPos((0,0))
        stim.draw()
        win.flip()
            
        
        thread.join()
        for i in range(17):
            if i >=7:
                thread = Thread(target = geturl, args = (url + 'GetSensorValue:[HandDynamometer],[1]',1)) #start the record of the left sensor
                thread.start()
                thread = Thread(target = geturl, args = (url + 'GetSensorValue:[HandDynamometer],[2]',2)) #start the record of the left sensor
                thread.start()    
            waiting =0
            start_waiting = time.time()
            while waiting <= 0.1:
                end_waiting = time.time()
                waiting = end_waiting - start_waiting
                
                
        ###################### THE TWO STIM ##################################################
        for finding_index in range(3):
            #baloon
            if cbs_index_shuffle[trial][0] in baloon_color_corresponding_value[finding_index]:
                orig_pos_baloon_left = finding_index * 15
                color_baloon_left = finding_index # which color is the left baloon
            if cbs_index_shuffle[trial][1] in baloon_color_corresponding_value[finding_index]:
                orig_pos_baloon_right = finding_index * 15
                color_baloon_right = finding_index # which color is the right baloon
              
            #token  
            if cbs_index_shuffle_token[trial][0] in token_color_corresponding_value[finding_index]:
                orig_pos_token_left = finding_index * 15
                color_token_left = finding_index
            if cbs_index_shuffle_token[trial][1] in token_color_corresponding_value[finding_index]:
                orig_pos_token_right = finding_index * 15
                color_token_right = finding_index
            
        orig_pos_baloon_left+= cbs_shuffle[trial][0]-1 # cbs_shuffle represent the value level of the stim, orig_baloon represent if it is the first 15th stim or next 15-30 or 30- 45. As python use -1 index we have to substract 1
        orig_pos_baloon_right+= cbs_shuffle[trial][1]-1
        orig_pos_token_left+= cbs_shuffle_token[trial][0]-1
        orig_pos_token_right+= cbs_shuffle_token[trial][1]-1

        #draw the stim baloon
        All_Baloon[orig_pos_baloon_left].setPos((-0.3,0)) #draw baloon
        All_Baloon[orig_pos_baloon_left].setSize((0.6,1.8)) #draw baloon
        All_Baloon[orig_pos_baloon_left].draw()
        All_Baloon[orig_pos_baloon_right].setPos((0.3,0))
        All_Baloon[orig_pos_baloon_right].setSize((0.6,1.8)) #draw baloon
        All_Baloon[orig_pos_baloon_right].draw()
        #draw the stim token
        token.setPos((-0.3,0.2))
        token.setSize((0.25,0.5))
        token.draw()
        stim.setText(cbs_shuffle_token[trial][0]) #draw number
        stim.setColor(gauge_color_token[orig_pos_token_left])
        stim.setHeight(0.3)
        stim.setPos((-0.32,0.21)) 
        stim.draw()
        token.setPos((0.3,0.2))
        token.draw()
        stim.setText(cbs_shuffle_token[trial][1])
        stim.setColor(gauge_color_token[orig_pos_token_right])
        stim.setPos((0.28,0.21)) #draw numbers on baloon
        stim.draw()  
        win.flip()
        
        ###################### RECORD THE ANSWER ##################################################

        #record the RT of the answer, escape possible to quit the task
        start = time.time()


        value_sensors_1 = [0]
        value_sensors_2 = [0]
        handpress_without_trigger()
        thread.join()
        
        end = time.time()

        if keypress == 'left':
            asw_baloon.append(orig_pos_baloon_left) #which value was selected
            asw_token.append(orig_pos_token_left) 
            circle.setPos((-0.32,0.05))
         
        elif keypress == 'right':
            asw_baloon.append(orig_pos_baloon_right)  # which baloon-color is selected
            asw_token.append(orig_pos_token_right) #which token-coler is selected
            circle.setPos((0.3,0.05))
        
    
        ###################### BIP AND CIRCLE ANSWER ##################################################
        time.sleep(randomBeep[trial])
        # draw the stim with circle answers
        thread_bip = Thread(target = winsound.Beep , args = (550,500))
        thread_bip.start() 
        time.sleep(0.55) #there is always a delay between visual and auditive stim, if we do not delay during 200ms the visual stim seems to appear before the beep
        
        All_Baloon[orig_pos_baloon_left].setPos((-0.3,0)) #draw baloon
        All_Baloon[orig_pos_baloon_left].setSize((0.6,1.8)) #draw baloon
        All_Baloon[orig_pos_baloon_left].draw()
        All_Baloon[orig_pos_baloon_right].setPos((0.3,0))
        All_Baloon[orig_pos_baloon_right].setSize((0.6,1.8)) #draw baloon
        All_Baloon[orig_pos_baloon_right].draw()
        #draw the stim token
        token.setPos((-0.3,0.2))
        token.setSize((0.25,0.5))
        token.draw()
        stim.setText(cbs_shuffle_token[trial][0]) #draw number
        stim.setColor(gauge_color_token[orig_pos_token_left])
        stim.setHeight(0.3)
        stim.setPos((-0.32,0.21)) 
        stim.draw()
        token.setPos((0.3,0.2))
        token.draw()
        stim.setText(cbs_shuffle_token[trial][1])
        stim.setColor(gauge_color_token[orig_pos_token_right])
        stim.setPos((0.28,0.21)) #draw numbers on baloon
        stim.draw()  
        circle.draw() #draw the circle
        win.flip()


        time.sleep(0.5) #present the circled answer
        ###################### TIME ESTIMATION ANSWER ##################################################
        cursor.setPos((0,-0.2))
        cursor.setVisible(1) #make the mouse appear to answer
        while not slider.rating:
            slider.draw()
            Beep.draw()
            Keypress.draw()
            interval_msg.draw()
            win.flip()        
        slider.reset()

        cursor.setVisible(0) # make the mouse disappear to avoid distractions

        
        # Change Gauge depending on the answer. baloon gauges mean the gauges corresponding to the baloon. token mean the gauges corresponding to the numbers.
        aws_index_baloon_gauges = asw_baloon[-1]
        aws_index_token_gauges = asw_token[-1]
   
        if aws_index_baloon_gauges<15 : 
            mode_baloon= 0 
            gauge_new_baloon = gauge_actual[0] +  asw_baloon[-1]
            if gauge_new_baloon>16 : 
                gauge_reward_baloon = 17
                gauge_new_baloon = gauge_new_baloon - 17
            else :
                gauge_reward_baloon = gauge_new_baloon           
 
        elif aws_index_baloon_gauges >14 and aws_index_baloon_gauges<30:
            mode_baloon = 1
            gauge_new_baloon = gauge_actual[1] + (asw_baloon[-1]-15)
            if gauge_new_baloon>34 : 
                gauge_reward_baloon = 35
                gauge_new_baloon = gauge_new_baloon - 17
            else :
                gauge_reward_baloon = gauge_new_baloon
        elif aws_index_baloon_gauges> 29:
            mode_baloon = 2
            gauge_new_baloon = gauge_actual[2] + (asw_baloon[-1]-30)
            if gauge_new_baloon>52 : 
                gauge_reward_baloon = 53
                gauge_new_baloon = gauge_new_baloon  - 17
            else :
                gauge_reward_baloon = gauge_new_baloon
        
        if aws_index_token_gauges <15:
            mode_token = 3
            gauge_new_token = gauge_actual[3] + asw_token[-1]
            if gauge_new_token>70 : 
                gauge_reward_token = 71
                gauge_new_token= gauge_new_token  - 17
            else :
                gauge_reward_token = gauge_new_token
        elif aws_index_token_gauges >14 and aws_index_token_gauges<30:
            mode_token = 4
            gauge_new_token = gauge_actual[4] + (asw_token[-1]-15)
            if gauge_new_token>88 : 
                gauge_reward_token = 89
                gauge_new_token = gauge_new_token - 17
            else :
                gauge_reward_token = gauge_new_token
        elif aws_index_token_gauges> 29:
            mode_token = 5
            gauge_new_token = gauge_actual[5] + (asw_token[-1]-30)
            if gauge_new_token>106 : 
                gauge_reward_token = 107
                gauge_new_token = gauge_new_token - 17
            else :
                gauge_reward_token = gauge_new_token
   
        if gauge_new_baloon != gauge_reward_baloon:
            stim2.setColor('white') #to make the +1 visible
            pos_plus1 = list(gauge_pos[i_random[0].index(mode_baloon)]) #to set the pos of the +1 depending of the gauge filled, list because tuple are not callable for the next code line
            pos_plus1[1] = pos_plus1[1]-0.45 # to display the +1 under the gauge
            stim2.setPos((pos_plus1))

        if gauge_new_token != gauge_reward_token:
            stim3.setColor('white')
            pos_plus1 = list(gauge_pos[i_random[1].index(mode_token)+3])
            pos_plus1[1] = pos_plus1[1]-0.45
            stim3.setPos((pos_plus1))
   
        ###################### Change the actual gauge with the new one ##################################################
        
        #change gauge_actual[mode_baloon] = gauge_new_baloon and for token
        gauge_actual[mode_baloon] = gauge_new_baloon
        gauge_actual[mode_token] = gauge_new_token
                         
    
        

                
        key = event.getKeys(keyList = 'escape')
        if len(key) > 0:
                save_and_quit()
                
        #change gauge_actual[mode_baloon] = gauge_new_baloon and for token
        gauge_actual[mode_baloon] = gauge_new_baloon
        gauge_actual[mode_token] = gauge_new_token
        if trial < nb_trials_training -1 : #add trials until the avant dernier
            trial+=1
        else :  # after the last trial ask if participant continue or not the training
            stim.setHeight(0.15)
            stim.setPos((0,0))
            stim.setColor("white")    
            stim.setText("Pour continuer l'entraînement appuyer sur 'Espace' sinon appuyer sur 'Entrée'")
            stim.draw()
            win.flip()
            key = event.waitKeys(keyList = ['return', 'space'])
            if key[0]=='return' :
                trial+=1
            elif key[0]=='space' : 
              trial = nb_trials_training-nb_trials_more

##########################################################################################################################################
########################## TASK #########################################################################################################
#########################################################################################################################

stim2.setColor("black") #make the +1 invisible except if a new gauge is filled
stim3.setColor("black")

trial= 0 #iterative of trial
for nb_conditions in range(len(conditions)) : # Training - Arbitrary - Midly deliberate - Fully deliberate
    condition = conditions[nb_conditions]
    value_gauge_condition = value_gauge[condition]
    value_gauge_condition_real = value_gauge_real[condition]
    
    random_value_baloon = 0 #to randomize value of unknown value
    random_value_token = 0 #to randomize value of unknown value
    if value_gauge_condition[0][0]==0:
        random_value_baloon = 1
    if value_gauge_condition[1][0]==0:
        random_value_token = 1
        
    #attribute randomly the 20-30-40 value to the colors of baloon and token
    baloon_color_corresponding_value = index_all
    token_color_corresponding_value = index_all


    trial_condition= 0 #iterative of trial inside the condition

    
    for nb_trial in range(nb_trials): #number of trial
        ###################### Draw the 6 gauges level ##################################################
        #draw 6 gauges
        if trial_condition == 0 :    
            reward = 0 #to display a reward value at the end of the condition
            stim.setHeight(0.15)
            stim.setPos((0,0))
            stim.setColor("white")
            stim.setText("Début de la tâche : "+ name_condition[condition] +".  Appuyer sur 'entrée'.")
            stim.draw()
            win.flip()
            event.waitKeys(keyList='return')
            gauge_actual = [0, 18, 36, 54, 72, 90] 
            
        stim.setHeight(0.15) #txt for the value inside
        stim.setColor('black')
    

                
        for i in range(6):
            if i < 3: # to display the value on the system up and down
                stim.setText(value_gauge_condition[0][i])
                gauge_stim[gauge_actual[i_random[0][i]]].setPos(gauge_pos[i])
                gauge_stim[gauge_actual[i_random[0][i]]].draw() 
            else : 
                stim.setText(value_gauge_condition[1][i-3])
                gauge_stim[gauge_actual[i_random[1][i-3]]].setPos(gauge_pos[i])
                gauge_stim[gauge_actual[i_random[1][i-3]]].draw()                 
                
            stim.setPos(value_pos[i])
            stim.draw()
        
        stim2.draw()  #draw the +1 on a baloon
        stim3.draw() #draw the +1 on a token         
        neutral_baloon.draw()
        token.setPos((-0.75,-0.55))
        token.setSize((0.3,0.4))
        token.draw()
        line.draw()
        stim.setHeight(0.15) #txt for the value stock reward
        win.flip()
        stim2.setColor("black") #make the +1 invisible except if a new gauge is filled
        stim3.setColor("black")
        
        cursor.setPos((-1,-1))
        
        value_sensors_1 = [0]
        value_sensors_2 = [0]
        handpress_without_trigger()
        
        key = event.getKeys(keyList = 'escape')
        if len(key) > 0:
                save_and_quit()
                
        ###################### THE FIXATION CROSS ##################################################
        stim.setText('+') #fixation cross
        stim.setColor('white')
        stim.setHeight(0.1)
        stim.setPos((0,0))
        stim.draw()
        win.flip()
        
        
        thread.join()
        for i in range(17):
            if i >=7:
                thread = Thread(target = geturl, args = (url + 'GetSensorValue:[HandDynamometer],[1]',1)) #start the record of the left sensor
                thread.start()
                thread = Thread(target = geturl, args = (url + 'GetSensorValue:[HandDynamometer],[2]',2)) #start the record of the left sensor
                thread.start()    
            waiting =0
            start_waiting = time.time()
            while waiting <= 0.1:
                end_waiting = time.time()
                waiting = end_waiting - start_waiting
            
            
        ###################### THE TWO STIM ##################################################
        
        # check which color is the baloon/token depending of the index of the current stim and create a start position of 0 of the color 0 Red 14 Green 28 Blue, then add the value filling to draw the good baloon color-level filling 
        for finding_index in range(3):
            #baloon
            if cbs_index_shuffle[trial][0] in baloon_color_corresponding_value[finding_index]:
                orig_pos_baloon_left = finding_index * 15
                color_baloon_left = finding_index # which color is the left baloon
            if cbs_index_shuffle[trial][1] in baloon_color_corresponding_value[finding_index]:
                orig_pos_baloon_right = finding_index * 15
                color_baloon_right = finding_index # which color is the right baloon
              
            #token  
            if cbs_index_shuffle_token[trial][0] in token_color_corresponding_value[finding_index]:
                orig_pos_token_left = finding_index * 15
                color_token_left = finding_index
            if cbs_index_shuffle_token[trial][1] in token_color_corresponding_value[finding_index]:
                orig_pos_token_right = finding_index * 15
                color_token_right = finding_index
               
        orig_pos_baloon_left+= cbs_shuffle[trial][0]-1 # cbs_shuffle represent the value level of the stim, orig_baloon represent if it is the first 15th stim or next 15-30 or 30- 45. As python use -1 index we have to substract 1
        orig_pos_baloon_right+= cbs_shuffle[trial][1]-1
        orig_pos_token_left+= cbs_shuffle_token[trial][0]-1
        orig_pos_token_right+= cbs_shuffle_token[trial][1]-1
        #draw the stim baloon
        All_Baloon[orig_pos_baloon_left].setPos((-0.3,0)) #draw baloon
        All_Baloon[orig_pos_baloon_left].setSize((0.6,1.8)) #draw baloon
        All_Baloon[orig_pos_baloon_left].draw()
        All_Baloon[orig_pos_baloon_right].setPos((0.3,0))
        All_Baloon[orig_pos_baloon_right].setSize((0.6,1.8)) #draw baloon
        All_Baloon[orig_pos_baloon_right].draw()
        #draw the stim token
        token.setPos((-0.3,0.2))
        token.setSize((0.25,0.5))
        token.draw()
        stim.setText(cbs_shuffle_token[trial][0]) #draw number
        stim.setColor(gauge_color_token[orig_pos_token_left])
        stim.setHeight(0.3)
        stim.setPos((-0.32,0.21)) 
        stim.draw()
        token.setPos((0.3,0.2))
        token.draw()
        stim.setText(cbs_shuffle_token[trial][1])
        stim.setColor(gauge_color_token[orig_pos_token_right])
        stim.setPos((0.28,0.21)) #draw numbers on baloon
        stim.draw()  
        win.flip()
        send_trigger(trigger_stim[condition][0])
        
        ###################### RECORD THE ANSWER ##################################################
        
        #record the RT of the answer, escape possible to quit the task
        start = time.time()
        
        value_sensors_1 = [0]
        value_sensors_2 = [0]
        handpress()
            
        end = time.time()
        RT=end-start

        if keypress == 'left':
            asw_baloon.append(orig_pos_baloon_left) #which value was selected
            asw_token.append(orig_pos_token_left) 
            value_baloon_selected.append(cbs_shuffle[trial][0])  #which baloon-value is selected
            value_token_selected.append(cbs_shuffle_token[trial][0]) #which token value is selected
            value_baloon_not_selected.append(cbs_shuffle[trial][1]) #which baloon-value is not selected
            value_token_not_selected.append(cbs_shuffle_token[trial][1]) 
            
            
            hand_sensors_answ.append(0) #stock the side of the hand (left/right)

            circle.setPos((-0.32,0.05))
            if random_value_baloon==0:
                real_value_baloon_selected.append(cbs_value_shuffle[trial][0])  #which baloon-real_value is selected       
                real_value_baloon_not_selected.append(cbs_value_shuffle[trial][1])  #which baloon-real_value is selected       
                final_reward = final_reward + cbs_value_shuffle[trial][0]
            else:
                real_value_baloon_selected.append((cbs_shuffle[trial][0]/17)*value[random.randrange(3)])  #choose a random value 20-30-40 for the stim     
                real_value_baloon_not_selected.append((cbs_shuffle[trial][1]/17)*value[random.randrange(3)])  #choose a random value 20-30-40 for the stim                       
                change_value_baloon_selected = real_value_baloon_selected[-1] #took the random value as the change
                change_value_baloon_not_selected = real_value_baloon_not_selected[-1] #took the random value as the change
                # final_reward = final_reward + change_value_baloon_selected
                
            if random_value_token==0:  
                real_value_token_selected.append(cbs_value_shuffle_token[trial][0]) #which token real_value is selected
                real_value_token_not_selected.append(cbs_value_shuffle_token[trial][1]) 
                final_reward = final_reward + cbs_value_shuffle_token[trial][0]
            else:
                real_value_token_selected.append((cbs_shuffle_token[trial][0]/17)*value[random.randrange(3)])      #choose a random value 20-30-40 for the stim
                real_value_token_not_selected.append((cbs_shuffle_token[trial][1]/17)*value[random.randrange(3)])   #choose a random value 20-30-40 for the stim                      
                change_value_token_selected =real_value_token_selected[-1]
                change_value_token_not_selected =real_value_token_not_selected[-1]
                # final_reward = final_reward + change_value_token_selected
            
        elif keypress == 'right':
            asw_baloon.append(orig_pos_baloon_right)  # which baloon-color is selected
            asw_token.append(orig_pos_token_right) #which token-coler is selected
            value_baloon_selected.append(cbs_shuffle[trial][1])  #which baloon-value is selected
            value_token_selected.append(cbs_shuffle_token[trial][1]) #which token value is selected
            value_baloon_not_selected.append(cbs_shuffle[trial][0]) #which baloon-value is not selected
            value_token_not_selected.append(cbs_shuffle_token[trial][0]) 
             
            hand_sensors_answ.append(1) #stock the side of the hand (left/right)

            circle.setPos((0.3,0.05))
            if random_value_baloon==0:
                real_value_baloon_selected.append(cbs_value_shuffle[trial][1])  #which baloon-real_value is selected       
                real_value_baloon_not_selected.append(cbs_value_shuffle[trial][0])  #which baloon-real_value is selected       
                final_reward = final_reward + cbs_value_shuffle[trial][1]
            else:
                real_value_baloon_selected.append((cbs_shuffle[trial][1]/17)*value[random.randrange(3)])  #choose a random value 20-30-40 for the stim     
                real_value_baloon_not_selected.append((cbs_shuffle[trial][0]/17)*value[random.randrange(3)])  #choose a random value 20-30-40 for the stim                       
                change_value_baloon_selected = real_value_baloon_selected[-1] #took the random value as the change
                change_value_baloon_not_selected = real_value_baloon_not_selected[-1] #took the random value as the change
                # final_reward = final_reward + change_value_baloon_selected
                
            if random_value_token==0:  
                real_value_token_selected.append(cbs_value_shuffle_token[trial][1]) #which token real_value is selected
                real_value_token_not_selected.append(cbs_value_shuffle_token[trial][0]) 
                final_reward = final_reward + cbs_value_shuffle_token[trial][1]
            else:
                real_value_token_selected.append((cbs_shuffle_token[trial][1]/17)*value[random.randrange(3)])      #choose a random value 20-30-40 for the stim
                real_value_token_not_selected.append((cbs_shuffle_token[trial][0]/17)*value[random.randrange(3)])   #choose a random value 20-30-40 for the stim                      
                change_value_token_selected =real_value_token_selected[-1]
                change_value_token_not_selected =real_value_token_not_selected[-1]
                # final_reward = final_reward + change_value_token_selected
        
                
        #stock the max and min value for the two systems in order to count the ratio at the end of the block
        if random_value_baloon==0:
            max_reward = max_reward + max(cbs_value_shuffle[trial])
            min_reward = min_reward + min(cbs_value_shuffle[trial])
            
        if random_value_token==0:
            max_reward = max_reward + max(cbs_value_shuffle_token[trial])
            min_reward = min_reward + min(cbs_value_shuffle_token[trial])
            
        ###################### BIP AND CIRCLE ANSWER ##################################################
        time.sleep(randomBeep[trial])
        # draw the stim with circle answers
        thread_bip = Thread(target = winsound.Beep , args = (550,500))
        thread_bip.start() 
        time.sleep(0.6) #there is always a delay between visual and auditive stim, if we do not delay during 200ms the visual stim seems to appear before the beep
     
        All_Baloon[orig_pos_baloon_left].setPos((-0.3,0)) #draw baloon
        All_Baloon[orig_pos_baloon_left].setSize((0.6,1.8)) #draw baloon
        All_Baloon[orig_pos_baloon_left].draw()
        All_Baloon[orig_pos_baloon_right].setPos((0.3,0))
        All_Baloon[orig_pos_baloon_right].setSize((0.6,1.8)) #draw baloon
        All_Baloon[orig_pos_baloon_right].draw()

        #draw the stim token
        token.setPos((-0.3,0.2))
        token.setSize((0.25,0.5))
        token.draw()
        stim.setText(cbs_shuffle_token[trial][0]) #draw number
        stim.setColor(gauge_color_token[orig_pos_token_left])
        stim.setHeight(0.3)
        stim.setPos((-0.32,0.21)) 
        stim.draw()
        token.setPos((0.3,0.2))
        token.draw()
        stim.setText(cbs_shuffle_token[trial][1])
        stim.setColor(gauge_color_token[orig_pos_token_right])
        stim.setPos((0.28,0.21)) #draw numbers on baloon
        stim.draw()  
        circle.draw() #draw the circle
        win.flip()
        
        if randomBeep[trial]==0:
           send_trigger(trigger_beep[condition][0])
           useless = 0 #when comment send_trigger to dont have a bug
        elif randomBeep[trial]==0.3:
           send_trigger(trigger_beep[condition][1])
           useless = 0
        elif randomBeep[trial]==0.6:
           send_trigger(trigger_beep[condition][2])
           useless = 0
        
        thread.join()
        for i in range(5):
            if i >=7:
                thread = Thread(target = geturl, args = (url + 'GetSensorValue:[HandDynamometer],[1]',1)) #start the record of the left sensor
                thread.start()
                thread = Thread(target = geturl, args = (url + 'GetSensorValue:[HandDynamometer],[2]',2)) #start the record of the left sensor
                thread.start()    
            waiting =0
            start_waiting = time.time()
            while waiting <= 0.1:
                end_waiting = time.time()
                waiting = end_waiting - start_waiting
            
        ###################### TIME ESTIMATION ANSWER ##################################################
        cursor.setPos((0,-0.2))
        cursor.setVisible(1) #make the mouse appear to answer
        
        if hand_sensors_answ[-1]==0 :
           squeeze_sensors = round((max(value_sensors_1)/squeeze_max_left)*100,2)
        elif hand_sensors_answ[-1]==1:
           squeeze_sensors = round((max(value_sensors_2)/squeeze_max_right)*100,2)
        ite_present_slide = 0
        while not slider.rating:
            slider.draw()
            Beep.draw()
            Keypress.draw()
            interval_msg.draw()
            win.flip()
            if ite_present_slide==0:
                send_trigger(trigger_scale[condition][0]) #trigger presentaiton slide
                ite_present_slide+=1
        send_trigger(trigger_scale[condition][1]) #trigger click slider
       
           
        value_sensors_answ.append(squeeze_sensors) #stock the value of the squeezing strenght 

        # writer.writerow("1-Participant", "Age", "Block", "Condition", "RT_Answer", "Tps_Esti",  "Tps_Real", "Strength_Squeeze",  "Hand_0left_1right", "Duo_Baloon_Presented", "Duo_Token_Presented", "Value_Baloon_Selected","Value_Token_Selected","Value_Baloon_Not_Selected","Value_Token_Not_Selected"])

        writer.writerow([exp_info["1-Participant"], exp_info["2-Age"], nb_conditions, condition, trial, round(RT,2), round(slider.getRating()),round(randomBeep[trial]*1000), value_sensors_answ[-1], hand_sensors_answ[-1],[orig_pos_baloon_left,orig_pos_baloon_right],[orig_pos_token_left,orig_pos_token_right], round(value_baloon_selected[-1],2), round(value_token_selected[-1],2),round(value_baloon_not_selected[-1],2), round(value_token_not_selected[-1],2),round(real_value_baloon_selected[-1],2), round(real_value_token_selected[-1],2),round(real_value_baloon_not_selected[-1],2), round(real_value_token_not_selected[-1],2)])
        slider.reset()
        cursor.setVisible(0) #make the mouse appear to answer
        
        ###################### Change the level of the actual gauge adding the selected stim ##################################################
      
        aws_index_baloon_gauges = asw_baloon[-1]
        aws_index_token_gauges = asw_token[-1]
        # Change Gauge depending on the answer. baloon gauges mean the gauges corresponding to the baloon. token mean the gauges corresponding to the numbers.
        
        if aws_index_baloon_gauges<15 : 
            mode_baloon= 0 
            gauge_new_baloon = gauge_actual[0] +  asw_baloon[-1]
            if gauge_new_baloon>16 : 
                gauge_reward_baloon = 17
                gauge_new_baloon = gauge_new_baloon - 17
            else :
                gauge_reward_baloon = gauge_new_baloon           
 
        elif aws_index_baloon_gauges >14 and aws_index_baloon_gauges<30:
            mode_baloon = 1
            gauge_new_baloon = gauge_actual[1] + (asw_baloon[-1]-15)
            if gauge_new_baloon>34 : 
                gauge_reward_baloon = 35
                gauge_new_baloon = gauge_new_baloon - 17
            else :
                gauge_reward_baloon = gauge_new_baloon
        elif aws_index_baloon_gauges> 29:
            mode_baloon = 2
            gauge_new_baloon = gauge_actual[2] + (asw_baloon[-1]-30)
            if gauge_new_baloon>52 : 
                gauge_reward_baloon = 53
                gauge_new_baloon = gauge_new_baloon  - 17
            else :
                gauge_reward_baloon = gauge_new_baloon
        
        if aws_index_token_gauges <15:
            mode_token = 3
            gauge_new_token = gauge_actual[3] + asw_token[-1]
            if gauge_new_token>70 : 
                gauge_reward_token = 71
                gauge_new_token= gauge_new_token  - 17
            else :
                gauge_reward_token = gauge_new_token
        elif aws_index_token_gauges >14 and aws_index_token_gauges<30:
            mode_token = 4
            gauge_new_token = gauge_actual[4] + (asw_token[-1]-15)
            if gauge_new_token>88 : 
                gauge_reward_token = 89
                gauge_new_token = gauge_new_token - 17
            else :
                gauge_reward_token = gauge_new_token
        elif aws_index_token_gauges> 29:
            mode_token = 5
            gauge_new_token = gauge_actual[5] + (asw_token[-1]-30)
            if gauge_new_token>106 : 
                gauge_reward_token = 107
                gauge_new_token = gauge_new_token - 17
            else :
                gauge_reward_token = gauge_new_token
                
        ###################### create a +1 on the gauge that is filled entirely ##################################################
        
        if gauge_new_baloon != gauge_reward_baloon:
            if random_value_baloon == 0 :
                reward = reward + value_gauge_condition_real[0][i_random[0].index(mode_baloon)] #add the neuro value to the final reward
            stim2.setColor('white') #to make the +1 visible
            pos_plus1 = list(gauge_pos[i_random[0].index(mode_baloon)]) #to set the pos of the +1 depending of the gauge filled, list because tuple are not callable for the next code line
            pos_plus1[1] = pos_plus1[1]-0.45 # to display the +1 under the gauge
            stim2.setPos((pos_plus1))

        if gauge_new_token != gauge_reward_token:
            if random_value_token == 0 :
                reward = reward + value_gauge_condition_real[1][i_random[0].index(mode_baloon)] #add the neuro value to the final reward
            stim3.setColor('white')
            pos_plus1 = list(gauge_pos[i_random[1].index(mode_token)+3])
            pos_plus1[1] = pos_plus1[1]-0.45
            stim3.setPos((pos_plus1))
   
        ###################### Change the actual gauge with the new one ##################################################
        
        #change gauge_actual[mode_baloon] = gauge_new_baloon and for token
        gauge_actual[mode_baloon] = gauge_new_baloon
        gauge_actual[mode_token] = gauge_new_token
        
        stock_reward = reward + stock_reward_bis #stock reward_bis is the value behind. Each trial the reward is added but the value initial is the same during the block

        #reset the handgrip sensors and value

                
        trial_condition +=1
        trial+=1
    stock_reward_bis = stock_reward
    
    stim.setHeight(0.15) #txt for the value inside
    stim.setColor('black')
    
    for i in range(6):
        if i < 3: # to display the value on the system up and down
            stim.setText(value_gauge_condition[0][i])
            gauge_stim[gauge_actual[i_random[0][i]]].setPos(gauge_pos[i])
            gauge_stim[gauge_actual[i_random[0][i]]].draw() 
        else : 
            stim.setText(value_gauge_condition[1][i-3])
            gauge_stim[gauge_actual[i_random[1][i-3]]].setPos(gauge_pos[i])
            gauge_stim[gauge_actual[i_random[1][i-3]]].draw()   
            
        stim.setPos(value_pos[i])
        stim.draw()
    stim2.draw()  #draw the +1 on a baloon
    stim3.draw() #draw the +1 on a token
       
    neutral_baloon.draw()
    token.setPos((-0.75,-0.55))
    token.setSize((0.3,0.4))
    token.draw()
    line.draw()
    win.flip()
    
    stim2.setColor("black") #make the +1 invisible except if a new gauge is filled
    stim3.setColor("black")

    value_sensors_1 = [0]
    value_sensors_2 = [0]
          
    handpress_without_trigger()
    thread.join()
    value_sensors_1 = [0]
    value_sensors_2 = [0]
            
    key = event.getKeys(keyList = 'escape')
    if len(key) > 0:
            save_and_quit()
 
    max_reward_block = max_reward - max_reward_block
    final_reward_block = final_reward - final_reward_block
    min_reward_block = min_reward - min_reward_block
   
    if final_reward_block != 0 :
        final_reward_percentage_block = round((((final_reward_block - min_reward_block) / (max_reward_block-min_reward_block))*100)) 
    if condition==0 :
        final_reward_percentage_block = 100
        
    percentage_block.append(final_reward_percentage_block)
    
    stim.setHeight(0.15)
    stim.setPos((0,0))
    stim.setColor("white")
    stim.setText("Bravo, Vous avez gagné "+ str(reward) +" Neuros. Soit " + str(final_reward_percentage_block) +"% du maximum pour ce block. Appuyer sur 'entrée'")
    stim.draw()
    win.flip()
    event.waitKeys(keyList='return')

    if nb_conditions ==0 :
        stim.setHeight(0.1)
        stim.wrapWidth = 1
        stim.setPos((0,0))
        stim.setColor("white")
        stim.setText("Dans cette section nous somme intéressés par votre propre expérience durant la tâche. Il est important d’être le plus honnête et transparant possible sur la nature de ce que vous avez ressenti. Grâce à vos réponses nous pourrons avoir une plus grande compréhension scientifique du phénomène que l’on étudie. Appuyer sur 'entrée'")
        stim.draw()
        win.flip()
        event.waitKeys(keyList='return')        
        stim.setText("Vous allez avoir une série de propositions concernant votre expérience. Veuillez évaluer si vous êtes en accord avec chacunes des propositions,  vous pourrez les notez de ‘0’ signifiant que vous êtes en total désaccord, à ‘100’ signifiant que vous êtes totalment d’accord avec la proposition. Appuyer sur 'entrée'")
        stim.draw()
        win.flip()
        event.waitKeys(keyList='return') 
        stim.setText("Vous n’avez pas besoin de réfléchir longuement pour chaques propositions, cliquez seulement sur l’échelle à l’endroit qui semble refléter le mieux votre propre expérience. Appuyer sur 'entrée'.")
        stim.draw()
        win.flip()
        event.waitKeys(keyList='return') 
        stim.wrapWidth = 1.8
    
    cursor.setVisible(1)
    for question in range(len(questions_SOAS)):
        cursor.setPos((0,-0.1))
        while not slider2.rating: #start questionaires
            slider2.draw()
            questionnaire_slider.setText(questions_SOAS[question])
            questionnaire_slider.draw()
            stim.setHeight(0.1)
            stim.setColor("white")
            stim.setText("Pas du tout d'accord")
            stim.setPos((-0.73,-0.4))
            stim.draw()
            stim.setText("Totalement d'accord")
            stim.setPos((0.74,-0.4))
            stim.draw()
    
            win.flip()
        
        qstr[question] =round(slider2.getRating())
        slider2.reset()
    # writer_qstr.writerow(["Participant", "Age", "Block", "Condition","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11"])
    writer_qstr.writerow([exp_info["1-Participant"], exp_info["2-Age"], nb_conditions, condition, percentage_block[-1],qstr[0],qstr[1],qstr[2],qstr[3],qstr[4],qstr[5],qstr[6],qstr[7],qstr[8],qstr[9],qstr[10]])

    cursor.setVisible(0)

final_reward_euros = round((((final_reward - min_reward) / (max_reward-min_reward))*7 )+15,2) #calculate the percentage of final score regarding the max score possible and the min when system reward activated. Use this percentage adding to 10€ the minimum
stim.setHeight(0.15)
stim.setPos((0,0))
stim.setColor("white")
# stim.setText("Bravo, l'expérience est fini. Vous avez gagné "+ str(final_reward_euros) +" Euros €  !")
stim.setText("Vous allez répondre à 3 questions et l'expérience sera finie. Vous pouvez utiliser le clavier pour répondre aux question et appuyer sur la touche 'ctrl' de droite pour valider votre réponse. Les accents, majuscules et certaines touches spécifiques ne sont pas activés. Appuyer sur 'entrée' quand vous êtes prêt.")
stim.draw()
win.flip()
event.waitKeys(keyList='return')


#stim text
# #textbox for the last three questions
textbox = visual.TextBox2(win=win, text="Quelle a été votre stratégie pour choisir les lots durant les scénario avec x systèmes activé(s) ?", 
      pos=(0, 0), size= 1.5, letterHeight=0.05,
    font='Arial', units='norm', borderWidth=1.0,
    color='black', colorSpace='rgb',
    padding=0, alignment='center',
    anchor='center',
    fillColor='white', borderColor='green',
    flipHoriz=False, flipVert=False,
    editable=True,lineBreaking='uax14')

for question_final in random.sample(range(0, 3), 3):
    textbox.setText("Quelle a été votre stratégie pour choisir les lots durant les scénario avec " +str(question_final)+" systèmes activé(s) ?")
    while True:
        textbox.draw()
        win.flip()

        key = event.getKeys()
        if len(key)>0:
            if key[0]== 'rctrl' :
                key=[]
                textbox.text = textbox.text + ' ' # the -1 do not take into account the last character so we add a space at the end
                answer_final[question_final]= textbox.text[98:-1] #98 is the number of character of the question
                textbox.clear()
                break
win.close()
writer_qstion.writerow([exp_info["1-Participant"], exp_info["2-Age"], version, answer_final[0], answer_final[1],answer_final[2]])
print("#########################################################")
print("#########################################################")
print("             DONNEZ " + str(final_reward_euros) + "EUROS AU PARTICIPANT")
print("#########################################################")
print("#########################################################")

datafile.close()
datafile_qstr.close()
datafile_qstion.close()
win.close()
port.close()
core.quit()
event.clearEvents()
raise SystemExit()