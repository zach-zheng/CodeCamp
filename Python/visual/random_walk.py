from random import choice

class RandomWalk():
    """a random walk class"""

    def __init__(self, num_points=5000):
        """Attitudes of random_walk"""
        self.num_points = num_points

        #All of random walk begin from (0, 0)
        self.x_values = [0]
        self.y_values = [0]

    def fill_walk(self):
        """counting all points"""
        #continue walks, until list reach num_points
        while len(self.x_values) < self.num_points:
            #determining direction and distance of walk
            x_direction = choice([1, -1])
            x_distance = choice([0, 1, 2, 3, 4])
            x_step = x_direction * x_distance

            y_direction = choice([1, -1])
            y_distance = choice([0, 1, 2, 3, 4])
            y_step = y_direction * y_distance

            #deny steps on the origination
            if x_step ==0 and y_step == 0:
                continue

            #counting next points values of x and y
            next_x = self.x_values[-1] + x_step
            next_y = self.y_values[-1] + y_step

            self.x_values.append(next_x)
            self.y_values.append(next_y)
