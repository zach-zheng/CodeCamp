import unittest 
from survey import AnonymousSurvey

class TestAnonymousSurvey(unittest.TestCase):
    def setUp(self):
        #创建调查对象和答案组，供使用测试方法用
        question = "What language did you first learn to speak?"
        self.my_survey = AnonymousSurvey(question)
        self.responses = ['English', 'Spanish', 'Mandarin']
        #return super().setUp()
    def test_store_single_response(self):
    #     #Testing a single response which be stored        
        self.my_survey.store_response(self.responses[0])
        self.assertIn(self.responses[0], self.responses)

    def test_store_three_response(self):
        for response in self.responses:
            self.my_survey.store_response(response)
        for response in self.responses:
            self.assertIn(response, self.responses)

    # To test AnonymousSurvey class #
    # def test_store_single_response(self):
    #     #Testing a single response which be stored
    #     question = "What language did you first learn to speak?"
    #     my_survey = AnonymousSurvey(question)
    #     my_survey.store_response('English')

    #     self.assertIn('English', my_survey.responses)

    # def test_store_three_responses(self):
    #     #Testing triple responses which be stored
    #     question = "What language did you first learn to speak?"
    #     my_survey = AnonymousSurvey(question)
    #     responses = ['English', 'Spanish', 'Mandarin']
    #     for response in responses:
    #         my_survey.store_response(response)

    #     for response in responses:
    #         self.assertIn(response, my_survey.responses)


unittest.main()