volatile int flow_frequency; // с помощью этой переменной мы будем подсчитывать импульсы от датчика расходы воды
// Calculated litres/hour
 float vol = 0.0,l_minute;
unsigned char flowsensor = 2; // Sensor Input
unsigned long currentTime;
unsigned long cloopTime;

void flow () // функция обработки прерывания
{
   flow_frequency++;
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
  if (Serial.available() > 0) {
    String str = Serial.readString();
    Serial.setTimeout(5);
    if (str[0] == 'S') {
      flow_frequency = 0;
      str.remove(0,1);
      float d = (float)str.toInt();
      float dd = (d - d*0.27) / 1000;
      while (vol < dd) {
        currentTime = millis();
        if(currentTime >= (cloopTime + 100)) {
          cloopTime = currentTime;
          if(flow_frequency != 0) {
            l_minute = (flow_frequency / 7.5);
            l_minute = l_minute/60;
            vol = vol +l_minute;
            Serial.println("P" + (String)(vol*1000));
            flow_frequency = 0; // сбрасываем счетчик
          }
        }
      }
      delay(100);
      Serial.println("F" + (String)d);
    }
    else {
      if (str == "UID") {
        Serial.print("YF-S201");
      }
      if (str == "PING") {
        Serial.print("OK-1");
      }
    }
    vol = 0.0;
    str = "";
    Serial.flush();
  }
}
