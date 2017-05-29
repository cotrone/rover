#!/usr/bin/env python

import paho.mqtt.client as mqtt
from threading import Thread
from time import sleep
import Adafruit_PCA9685
from struct import *


def dummySetPwm(pwmPort, minVal, maxVal):
    print(pwmPort, minVal, maxVal)

try:
    pwm = Adafruit_PCA9685.PCA9685(busnum=0)
    setPwm = pwm.set_pwm
except IOError:
    setPwm = dummySetPwm  

driveMin = 150  # Min pulse length out of 4096
driveMax = 600  # Max pulse length out of 4096
driveMid = (driveMax - driveMin) / 2

writes = {'val':0}

def setWheelsMid():
    for i in range(5):
        setPwm(i,0,driveMid)

def watchThread():
    lastWrites = writes['val']
    while True:
        sleep(0.5)
        if writes['val'] <= lastWrites:
            print("Timeout")
            setWheelsMid()
        lastWrites = writes['val']

def on_connect(client, userdata, flags, rc):
    print("Connected with result code" + str(rc))
    client.subscribe("drive")

def setWheelValue(channel, value):
    setPwm(channel, 0 , driveMid + value)

def on_disconnect(client, userdata, rc):
    setWheelsMid()

def callback(client, userdata, msg):
    (fl, ml, bl, fr, mr, br) = unpack('>hhhhhh', msg.payload)
    setWheelValue(0, int(ml))
    setWheelValue(1, int(mr))
    setWheelValue(2, int(fr))
    setWheelValue(3, int(fl))
    setWheelValue(4, int(br))
    setWheelValue(5, int(bl))
    writes['val'] = writes['val'] + 1

def listener():
    client = mqtt.Client()
    client.on_connect = on_connect
    client.on_message = callback
    client.on_disconnect = on_disconnect
    client.connect("192.168.0.102", 1883, 60)

    print("Starting ros drive node")
    
    client.loop_forever()

if __name__ == '__main__':
    listener()