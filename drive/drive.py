#!/usr/bin/env python

import paho.mqtt.client as mqtt
from threading import Thread
from time import sleep
import Adafruit_PCA9685
from struct import *


def dummySetPwm(pwmPort, minVal, maxVal):
    print(pwmPort, minVal, maxVal)

try:
    pwm = Adafruit_PCA9685.PCA9685(busnum=1)
    pwm.set_pwm_freq(100)
    setPwm = pwm.set_pwm
    print("Started with real")
except IOError as err:
    setPwm = dummySetPwm  
    print("Error connecting: ", err)

driveMin = 550  # Min pulse length out of 4096
driveMax = 750  # Max pulse length out of 4096
driveMid = 660 # driveMin + ((driveMax - driveMin) / 2)

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
    print(channel,value)
    setPwm(channel, 0 , driveMid + value)

def on_disconnect(client, userdata, rc):
    print("Disconnected")
    setWheelsMid()

def callback(client, userdata, msg):
    (fl, ml, bl, fr, mr, br) = unpack('>hhhhhh', msg.payload)
    print(fl,ml,bl,fr,mr,br)
    setWheelValue(0, int(100 * bl/32767))
    setWheelValue(1, -int(100 * fl/32767))
    setWheelValue(2, -int(100 * ml/32767))
    setWheelValue(3, -int(100 * mr/32767))
    setWheelValue(4, -int(100 * fr/32767))
    setWheelValue(5, int(100 * br/32767))
    writes['val'] = writes['val'] + 1

def listener():
    client = mqtt.Client()
    client.on_connect = on_connect
    client.on_message = callback
    client.on_disconnect = on_disconnect
    client.connect("192.168.0.100", 1883, 60)

    print("Starting ros drive node")
    
    client.loop_forever()

def set_servo_pulse(channel, pulse):
    pulse_length = 1000000    # 1,000,000 us per second
    pulse_length //= 500      # 60 Hz
    print('{0}us per period'.format(pulse_length))
    pulse_length //= 4096     # 12 bits of resolution
    print('{0}us per bit'.format(pulse_length))
    pulse *= 1000
    pulse //= pulse_length
    pwm.set_pwm(channel, 0, pulse)

if __name__ == '__main__':
    print("Mid: ", driveMid)
    listener()
