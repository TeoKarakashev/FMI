//YWROBOT
//Compatible with the Arduino IDE 1.0
//Library version:1.1
#include <Wire.h> 
#include <LiquidCrystal_I2C.h>

LiquidCrystal_I2C lcd(0x27,16,2);  // set the LCD address to 0x27 for a 16 chars and 2 line display

void setup()
{
  Wire.begin(13,14);
  lcd.init();                      // initialize the lcd 
}


void loop(){
  lcd.backlight();
  lcd.setCursor(0,0);
  lcd.print("Hello, world!");
  lcd.setCursor(0,1);
  lcd.print("Ywrobot Arduino!");
  delay(1000);
  lcd.setCursor(0,0);
  lcd.print("Arduino LCM IIC 2004");
  lcd.setCursor(0,1);
  lcd.print("Power By Ec-yuan!");
  delay(1000);
}