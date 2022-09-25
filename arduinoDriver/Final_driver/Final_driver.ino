#define IDL 1
#define LIQUID_ON 2
#define SENDING_FINISH 3
#include <HCSR04.h>
HCSR04 hc(4, 5);

volatile int flow_frequency; // with this variable, we will count the pulses from the water flow sensor
float vol = 0.0,l_minute, requiredVol, requiredVoln, resultDistance;
int stat = IDL;
unsigned char flowsensor = 2; // Sensor Input
unsigned long currentTime, cloopTime;
int timer = 200;

void flow () // interrupt handling function
{
   flow_frequency++;
}

void checkVolume () {
  l_minute = (flow_frequency / 7.5);
  l_minute = l_minute/60;
  vol = vol + l_minute;
  flow_frequency = 0; // resetting the counter
}

void CheckDistance () {
  float v = hc.dist();
    if (resultDistance == 0) {
      resultDistance = v;
    }
    if (v != 0) {
      resultDistance = resultDistance * 0.9 + v * 0.1;
    }
}

void setup()
{
   pinMode(flowsensor, INPUT);
   digitalWrite(flowsensor, HIGH); // Optional Internal Pull-Up
   Serial.begin(38400);

   attachInterrupt(digitalPinToInterrupt(flowsensor), flow, RISING); // Setup Interrupt
   currentTime = millis();
   cloopTime = currentTime;
}

void loop() {  
  if (stat == LIQUID_ON) {
        checkVolume();
        currentTime = millis();
        if (requiredVol > vol) {
          if(currentTime >= (cloopTime + timer)) {
            cloopTime = currentTime;
            Serial.println("P" + (String)(int)(vol*1000 + (vol*1000*0.27)) + ";");
          } 
        } else {
          stat = SENDING_FINISH;
        }
  }
  if (stat == SENDING_FINISH) {
    currentTime = millis();
    if (currentTime >= (cloopTime + timer)) {
      cloopTime = currentTime;
      Serial.println("F" + (String)(int)requiredVoln + ";");
    }
  }
  if (Serial.available() > 0) {
    String str = Serial.readString();
    Serial.setTimeout(5);
    if (str[0] == 'S') {
      Serial.println("SOK;");
      str.remove(0,1);
      stat = LIQUID_ON;
      vol = 0.0;
      requiredVoln = (float)str.toInt();
      requiredVol = (requiredVoln - requiredVoln*0.27) / 1000;
    } else {
      if (str == "L;") {
        Serial.println(int(itog));
      }
      if (str == "UID;") {
        Serial.print("YF-S201;");
      }
      if (str == "PING;") {
        CheckDistance();
        Serial.println("OK-PING;");
      }
      if (str == "ERR;") {
        stat = IDL;
        Serial.println("FOK;");
      }
      if (str == "FOK;") {
        stat = IDL;
      }
    }
    str = "";
    Serial.flush();
  }
}
