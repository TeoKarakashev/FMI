#include <Wire.h>
#include "C:\Users\teodor\Documents\Arduino\libraries\LiquidCrystal_I2C\LiquidCrystal_I2C.h"
#include "C:\Users\teodor\Documents\Arduino\libraries\Adafruit_VL53L0X\src\Adafruit_VL53L0X.h"
#include "C:\Users\teodor\Documents\Arduino\libraries\NeoPixelBus_by_Makuna\src\NeoPixelBus.h"
#include "C:\Users\teodor\Documents\Arduino\libraries\Adafruit_BME280_Library\Adafruit_BME280.h"
#include <Adafruit_Sensor.h>

const uint16_t PixelCount = 16; // this example assumes 4 pixels, making it smaller will cause a failure
const uint8_t PixelPin = 17;  // make sure to set this to the correct pin, ignored for Esp8266

#define Cred     RgbColor(255, 0, 0)
#define Cgreen   RgbColor(0, 255, 0)
#define Corange  RgbColor(255, 164, 0)
#define Cwhite   RgbColor(255, 255, 255)
#define Cblack   RgbColor(0)
RgbColor allColors[] = { Cred, Corange, Cgreen, Cwhite };

Adafruit_BME280 bme; 
Adafruit_VL53L0X lox = Adafruit_VL53L0X();
NeoPixelBus<NeoGrbFeature, Neo800KbpsMethod> strip(PixelCount, PixelPin);

LiquidCrystal_I2C lcd(0x27, 16, 2);

void setup() {
    Serial.begin(115200);
    
    lcd.init();
    lcd.backlight();
    lcd.noBlink();
    strip.Begin();
    strip.Show();
    if (!lox.begin()) {
        while (1);
    }

    status = bme.begin();  
    if (!status) {
        while (1);
    }

    lox.startRangeContinuous();
}


void loop() {
    int distance = lox.readRange();

    printDisplay(distance);
    setColor(distance);
}

void printDisplay(int distance) {
    lcd.clear();
    lcd.setCursor(0, 0);
    lcd.printf("Distance: %d", distance);
    lcd.setCursor(0, 1);
    lcd.printf("Temp: %d °C", bme.readTemperature());
    Serial.println(distance);
    Serial.println(bme.readTemperature());
    lcd.print(distance);
}

void setColor(int distance) {
    RgbColor c;
    if (distance < 150) {
        c = allColors[0];
    }
    else if (distance >= 150 && distance <= 300) {
        c = allColors[1];
    }
    else if (distance > 300 && distance <= 500) {
        c = allColors[2];
    }
    else {
        c = allColors[3];
    }

    for (int i = 0; i < PixelCount; i++) {
        strip.SetPixelColor(i, c);
    }
    strip.Show();
}

