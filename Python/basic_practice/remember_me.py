import json


# Get username if had it
def get_username():
    filename = 'username.json'
    try:
        with open(filename) as f_obj:
            username = json.load(f_obj)           
    except FileNotFoundError:
        return None  
    else:
        return username

def greet_new_user():
    #greeting user and show name
    
    username = input("What's your name? ")
    filename = 'username.json'
    with open(filename, 'w') as f_obj:
        json.dump(username, f_obj)
        return username    

def greet_user():
    username = get_username()
    if username:
        print("Welcome back, " + username + "!")
    else:
        username = greet_new_user()
        print("We'll remember you when you come back, " + username + "!")

greet_user()
