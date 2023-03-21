#include <Wire.h> 
#include <LiquidCrystal_I2C.h>
#include <Adafruit_VL53L0X.h>

Adafruit_VL53L0X lox = Adafruit_VL53L0X();

LiquidCrystal_I2C lcd(0x27, 16, 2);  // set the LCD address to 0x27 for a 16 chars and 2 line display

int VCC2 = 23;

void setup() {
    Serial.begin(115200);
    lcd.init();                      // initialize the lcd 
    pinMode(VCC2, OUTPUT);
    digitalWrite(VCC2, HIGH);

    if (!lox.begin()) {
        while (1);
    }
    lox.startRangeContinuous();
}


void loop() {
    int distance = lox.readRange();

    lcd.backlight();
    lcd.clear();
    lcd.setCursor(0, 0);
    lcd.print("Distance:");
    lcd.setCursor(0, 1);
    Serial.println(distance);
    lcd.print(distance);
    delay(200);
}