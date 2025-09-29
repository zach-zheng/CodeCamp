import unittest
from name_f import get_formatted_name

class NamesTestCase(unittest.TestCase):
    #"""Testing name_funciton.py"""

    def test_first_last_name(self):
        """Can resolve the name of Janis Joplin? """
        formatted_name = get_formatted_name('janis', 'joplin')
        self.assertEqual(formatted_name, 'Janis Joplin')

    def test_f_l_m_name(self):
        #Can do it the name like 'Wolfgang Amadeus Mozart'? "
        formatted_name = get_formatted_name(
            'wolfgang', 'mozart', 'amadeus'
        )
        self.assertEqual(formatted_name, 'Wolfgang Amadeus Mozart')

unittest.main()