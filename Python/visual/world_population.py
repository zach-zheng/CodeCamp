import json
import pygal_maps_world.maps

from country_codes import get_country_code

wm = pygal_maps_world.maps.World()
#population data load to a list
filename = 'population_data.json'
with open(filename) as f_obj:
    pop_data = json.load(f_obj)

#print population data of every country in 2010
world_populations = {}
for pop_dict in pop_data:
    if pop_dict['Year'] == '2010':
        country_name = pop_dict['Country Name']
        population = int(float(pop_dict['Value']))
        code = get_country_code(country_name)
        if code:
            world_populations[code] = population
cc_pops_1, cc_pops_2, cc_pops_3 = {}, {}, {}
for cc, pop in world_populations.items():
    if pop < 10000000:
        cc_pops_1[cc] = pop
    elif pop < 1000000000:
        cc_pops_2[cc] = pop
    else:
        cc_pops_3[cc] = pop
    
print(len(cc_pops_1), len(cc_pops_2), len(cc_pops_3))

wm.title = 'World Population in 2010, by Country'
wm.add('0-10m', cc_pops_1)
wm.add('10m-1bn', cc_pops_2)
wm.add('>1bn', cc_pops_3)

wm.render_to_file('world_populations.svg')