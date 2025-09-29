from car import Car

class Battery():
    def __init__(self, battery_size = 230):
        self.battery_size = battery_size
    
    def describe_battery(self):
        print("This car has a " + str(self.battery_size) + "-kwh battery.")

    def get_rage(self):
        if self.battery_size == 230:
            range = 300
        elif self.battery_size == 240:
            range = 320
        message = "This car can go approximately " + str(range)
        message += " miles on a full charge."
        print(message)


class ElectricCar(Car):
    def __init__(self, make, model, year):
        super().__init__(make, model, year)
        self.battery = Battery()

    