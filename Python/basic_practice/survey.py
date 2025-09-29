class AnonymousSurvey():
    """收集匿名调查问卷的答案"""

    def __init__(self, question):
        """存储问题，并为存储答案做准备"""
        self.question = question
        self.responses = []
    
    def show_question(self):
        """show questions"""
        print(self.question)

    def store_response(self, new_response):
        """store responses"""
        self.responses.append(new_response)

    def show_results(self):
        """show responses of questions"""
        print("Survey results: ")
        for response in self.responses:
            print('- ' + response)