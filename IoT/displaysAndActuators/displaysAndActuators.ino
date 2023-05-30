#include <Wire.h>
#include "LiquidCrystal_I2C.h"
#include "Adafruit_VL53L0X.h"
#include "NeoPixelBus.h"
#include "Adafruit_BME280.h"
#include "Adafruit_Sensor.h"

#define BME_SCK 13
#define BME_MISO 12
#define BME_MOSI 11
#define BME_CS 10

#define SEALEVELPRESSURE_HPA (1013.25)

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
  while (!Serial);
  lcd.init();
  lcd.backlight();
  lcd.noBlink();
  strip.Begin();
  strip.Show();
  if (!lox.begin()) {
    while (1);
  }
  unsigned status;
  status = bme.begin(0x76);
  if (!status) {
    while (1);
  }

  lox.startRangeContinuous();
}


void loop() {
  int distance = lox.readRange();
  int temp = bme.readTemperature();
  printDisplay(distance, temp);
  setColor(distance);
  delay(200);
}

void printDisplay(int distance, int temp) {
  lcd.clear();
  lcd.setCursor(0, 0);
  lcd.print("Distance:");
  lcd.setCursor(10, 0);
  lcd.print(distance);
  lcd.setCursor(0, 1);
  lcd.printf("Temp:");
  lcd.setCursor(6, 1);
  lcd.print(temp);
  Serial.println(distance);
  Serial.println(temp);
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
