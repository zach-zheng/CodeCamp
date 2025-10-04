import pygal
from die import Die

#create 2 D6(everyone has 6 sides)
die_1 = Die()
die_2 = Die()
die_3 = Die()

#roll many times dice, and stored result into a list
results = []
for roll_num in range(1000):
    result = die_1.roll() + die_2.roll() + die_3.roll()
    results.append(result)

frequencies = []
max_result = die_1.num_sides + die_2.num_sides + + die_3.num_sides
for value in range(3, max_result+1):
    frequency = results.count(value)
    frequencies.append(frequency)

# visualize results
hist = pygal.Bar()
hist._title = "Results of rolling two D6 dice 1000 times."
hist.x_labels = ['3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18']
hist._x_title = "Results"
hist._y_title = "Frequency of Result"

hist.add('D6 + D6 + D6', frequencies)
hist.render_to_file('die_visual.svg')
# print(results)
print(frequencies) 