from car import Car
from electric_car import ElectricCar


my_new_car = Car('audi', 'a6', 2020)
print(my_new_car.get_descriptive_name())
my_new_car.update_odometer(7000)
my_new_car.read_odometer()

my_new_car.increment_odometer(200)
my_new_car.read_odometer()


my_byd = ElectricCar('BYD', 'tang', 2022)
print(my_byd.get_descriptive_name())

my_byd.battery.describe_battery()
my_byd.battery.get_rage()