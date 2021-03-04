import requests
from bs4 import BeautifulSoup
import sys
import time

MORSE_CODE_DICT = { 'A':'.-', 'B':'-...', 
                    'C':'-.-.', 'D':'-..', 'E':'.', 
                    'F':'..-.', 'G':'--.', 'H':'....', 
                    'I':'..', 'J':'.---', 'K':'-.-', 
                    'L':'.-..', 'M':'--', 'N':'-.', 
                    'O':'---', 'P':'.--.', 'Q':'--.-', 
                    'R':'.-.', 'S':'...', 'T':'-', 
                    'U':'..-', 'V':'...-', 'W':'.--', 
                    'X':'-..-', 'Y':'-.--', 'Z':'--..', 
                    '1':'.----', '2':'..---', '3':'...--', 
                    '4':'....-', '5':'.....', '6':'-....', 
                    '7':'--...', '8':'---..', '9':'----.', 
                    '0':'-----', ', ':'--..--', '.':'.-.-.-', 
                    '?':'..--..', '/':'-..-.', '-':'-....-', 
                    '(':'-.--.', ')':'-.--.-'} 

def transform(unicode_message):
    out = []
    for i, x in enumerate(unicode_message):
        if x == '–':
            out.append('-')
        elif x == '·':
            out.append('.')
        elif x == ' ' and unicode_message[i-1] == ' ':
            out.append('/')
        elif x == ' ':
            out.append(' ')
    return ''.join(out)

def decrypt(message):
    message += ' '
    decipher = ''
    citext = ''
    for letter in message:
        if letter == '/':
            decipher += ' '
            citext = ''
        elif letter == ' ':
            decipher += list(MORSE_CODE_DICT.keys())[list(MORSE_CODE_DICT.values()).index(citext)] 
            citext = ''
        else: 
            citext += letter
  
    return decipher 

def get_unicode_message(content):
    soup = BeautifulSoup(content, features='lxml')
    return soup.body.code.text

def get_decrypted_message(content):
    # We extract the unicode message out of the HTML content of the page.
    unicode_message = get_unicode_message(content)

    # We transform the unicode message into readable morse code.
    morsecode_message = transform(unicode_message)

    # We decrypt the morse-code message.
    decrypted_message = decrypt(morsecode_message)
    
    return decrypted_message



if __name__ == "__main__":
    
    if len(sys.argv) < 2:
        print("Please provide the URL of the game as a command-line argument.")
        sys.exit()
    
    url = sys.argv[1]
    
    # We will use a Session object by the 'requests' library so that we have
    # persistance across our requests.
    session = requests.Session()

    # First, we do a get-request to get the page of the game. In there, we 
    # find the 1st Question.
    req = session.get(url)

    while True:
        for i in range(12):

            # We read and decrypt the message.
            decrypted_message = get_decrypted_message(req.content)
            print("Decrypted Message", i+1, "\n" + decrypted_message + "\n")
            
            # We post the answer and "press" the submit button.
            req = session.post(
                url,
                data = {'submit': 'Submit!', 'answer': decrypted_message}
            )
        
            # We search the text of the page to see if the submission was correct
            # or not.
            if 'class="right"' in req.text:
                if i == 11:
                    break                
                req = session.post(url, data={'continue': 'continue'})
            elif 'class="wrong"' in req.text:
                print("Something's wrong!")
                break
            
            # Just so that we can see the answers get printed.
            time.sleep(0.5)
        
        # When we finish with the questions, we ask for another round.
        print('Wanna play again? (type "yes" or "no")')
        yes_or_no = input()
        
        if yes_or_no == "yes":
            print("Woohoo! Let's do this...")
            req = session.post(
                url,
                data = {'reset': 'reset'}
            )
        elif yes_or_no == "no":
            print("Ouch! That hurt...")
            break
        else:
            print("You speak with riddles old man... I'm gone.")
            break
