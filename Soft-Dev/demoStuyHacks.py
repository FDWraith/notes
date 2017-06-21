import time

TEST_CASE_1 = [[5, 10], [144, 10], [1000, 3], [2, 4], [64, 5], [8, 1]]

def mysqrt(number, digits):
    """"
    Takes:
        number(int): the number to approximate the square root of
        digits(int): the number of digits to approximate it to
    Returns:
        The approximation to the correct number of digits, as a string
    """
    low  = 0
    high = number
    guess = number/2.0
    record = [guess]

    #Ensure that record has at least three values to prevent index-out-of-range errors
    for i in range(2):
        if guess**2 > number: #too high
            high = guess
            guess = (guess + low)/2.0
            record.append(guess)
        elif guess**2 < number: #too low
            low = guess
            guess = (guess + high)/ 2.0
            record.append(guess)
        else:
            return str(guess) + ", exactly."
        
    #Approximate the square root until the specified number of digits stay constant.
    while digitlen(str(record[-1])) < digits + 2 or \
          digitlen(str(record[-2])) < digits + 2 or \
          digitlen(str(record[-3])) < digits + 2 or \
          str(record[-1])[:digits + 2] != str(record[-2])[:digits + 2]:
        if guess**2 > number: #too high
            high = guess
            guess = (guess + low)/2.0
            record.append(guess)
        elif guess**2 < number: #too low
            low = guess
            guess = (guess + high)/ 2.0
            record.append(guess)
        else:
            return str(guess) + ", exactly."
        
    if "." in str(record[-1]):
        return str(record[-1])[:digits + 1]
    else:
        return str(record[-1])[:digits]
            
            
def digitlen(number):
    if "." in number:
        return len(number) - 1
    else:
        return len(number)
    
def htmlify(approxs):
    """
    Formats the approximations into HTML
    Takes a list of lists in the form of:
        [number (int), digits (int), approximation (str)]
    Returns HTML code to display the results
    """
    data = "<html><body><center>"
    for i in approxs:
        data += "<br>"
        if "exactly" in i[2]:
            data += "The square root of {} is {}".format(str(i[0]), i[2])
        else:
            data += "The square root of {} approximated to {} digits is {}".format(str(i[0]), str(i[1]), i[2])
    data += "</center></body></html>"
    return data
        
def writetofile(html):
    """ Writes the HTML code to a file """
    with open("{}.html".format(int(time.time() * 1000)), "w") as file:
        file.write(html)
        
def driver(mylist):
    for i in mylist:
        i.append(mysqrt(i[0], i[1]))
    html = htmlify(mylist)
    writetofile(html)
                
driver(TEST_CASE_1)
