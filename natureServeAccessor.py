import requests
# used to make REST API calls
import json
# used to save data to file

with open('Troblobionts updated.csv', 'r') as file:
    # Open the csv in [r]ead mode
    names = [name.strip('\n') for name in file.readlines()[1:]]
    # file.readlines() returns a list with two problems:
    # 1) The first element is just the string "Species"
    # We solve this by slicing [1:] to get every element from index 1 on
    # 2) Each element ends in a newline character \n
    # We solve this by calling the strip() method of each element in a list comprehension

print(names[0:10])
# Just to check that it worked

# Now we need a JSON object to make up the API request body
# All of these parameters are listed in the NatureServe documentation under "The Search Criteria Object"
params = {'criteriaType': 'combined',
          # Docs say that 'species' should be valid here, but it's not
          'textCriteria': [{
              # Note that 'textCriteria' needs an array of criteria objects
              # In this case there's only one set of criteria at a time, hence [{}]: an array of one object
              'paramType': 'textSearch',
              # 'quickSearch' would also work
              'searchToken': '',
              # To be filled in when we loop through the names
              'matchAgainst': 'scientificName',
              'operator': 'equals'
           }]
          }

with open('troglobionts.json', 'w') as file:
    # Open the data file in [w]rite mode; this will create troglobionts.json if it doesn't exist already
    file.write('[')
    # Format as a JSON array; opening and closing brackets, commas inbetween
    for name in names:
        # Looping through the list we created from the .csv
        params['textCriteria'][0]['searchToken'] = name
        # params is our JSON object
        # params['textCriteria'] is the array with one object
        # params['textCriteria'][0] is the object in that array
        # params['textCriteria'][0]['searchToken'] is the token string that was empty to start with
        # This line will change the token on every loop

        data = requests.post(url=f'https://explorer.natureserve.org/api/data/search', json=params)
        # Now we've finally made an API call to the endpoint URL listed in the docs
        # and saved the response in a new variable called data
        data.raise_for_status()
        # Check for errors
        json.dump(data.json(), file, indent=2)
        # Save the data as text in the file we opened on line 34
        file.write(',')
    file.write(']')
    
