#!/usr/bin/env python
# Python script om P1 telegram weer te geven
 
import re
import serial
from time import strftime
 
# Seriele poort confguratie
ser = serial.Serial()
 
# DSMR 4.0/4.2 > 115200 8N1:
ser.baudrate = 115200
ser.bytesize = serial.EIGHTBITS
ser.parity = serial.PARITY_NONE
ser.stopbits = serial.STOPBITS_ONE
 
ser.xonxoff = 0
ser.rtscts = 0
ser.timeout = 12
ser.port = "/dev/ttyUSB0"
 
while True:
  ser.open()
  checksum_found = False
 
  while not checksum_found:
    telegram_line = ser.readline() # Lees een seriele lijn in.
    telegram_line = telegram_line.decode('ascii').strip()

    if re.match('(?=1-0:1.7.0)', telegram_line):
      kw = telegram_line[10:-4]
      global current_usage
      current_usage = float(kw) * 1000
      current_usage = int(current_usage)
    if re.match('(?=1-0:2.7.0)', telegram_line):
      kw = telegram_line[10:-4]
      global current_supply
      current_supply = float(kw) * 1000
      current_supply = int(current_supply)
    if re.match('(?=1-0:1.8.1)', telegram_line):
      kw = telegram_line[10:-5]
      global current_usage_level_low
      current_usage_level_low = float(kw)
    if re.match('(?=1-0:1.8.2)', telegram_line):
      kw = telegram_line[10:-5]
      global current_usage_level_high
      current_usage_level_high = float(kw)
    if re.match('(?=1-0:2.8.1)', telegram_line):
      kw = telegram_line[10:-5]
      global current_supply_level_low
      current_supply_level_low = float(kw)
    if re.match('(?=1-0:2.8.2)', telegram_line):
      kw = telegram_line[10:-5]
      global current_supply_level_high
      current_supply_level_high = float(kw)
    if re.match('(?=0-1:24.2.1)', telegram_line):
      global latest_gas_measurement
      latest_gas_measurement = telegram_line[11:-15]
      global current_gas_level
      current_gas_level = telegram_line[26:-4]

    if re.match('(?=!)', telegram_line):
      with open("slimmemeter-rpi/Data/EnergyVerbruik.txt", "a") as myfile:
        time = strftime("%Y-%m-%d %H:%M:%S")
##        print(time + "\t" + str(current_usage) + "\t" + str(current_supply)
##                   + "\t" + str(current_usage_level_low)+ "\t" + str(current_usage_level_high)
##                   + "\t" + str(current_supply_level_low)+ "\t" + str(current_usage_level_high)
##                   + "\t" + str(latest_gas_measurement)+ "\t" + str(current_gas_level)
##                   + "\n")
        myfile.write(time + "\t" + str(current_usage) + "\t" + str(current_supply)
                   + "\t" + str(current_usage_level_low)+ "\t" + str(current_usage_level_high)
                   + "\t" + str(current_supply_level_low)+ "\t" + str(current_usage_level_high)
                   + "\t" + str(latest_gas_measurement)+ "\t" + str(current_gas_level)
                   + "\n")
      checksum_found = True

  ser.close()
