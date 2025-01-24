import requests
import json

NAMES_FILE='troglobionts.csv'
OUTPUT_FILE='troglobionts_search_results.json'

ENDPOINT='https://explorer.natureserve.org/api/data/search'
# NatureServe Explorer Search API; this connects to a public database of species conservation data
# https://explorer.natureserve.org/api-docs/#_the_search_criteria_object

params = {'criteriaType': 'combined',
          # Documentaion says that 'species' should be valid here, but it's not
          'textCriteria': [{
              'paramType': 'textSearch',
              'searchToken': '',
              # To be filled in when we loop through the names
              'matchAgainst': 'scientificName',
              'operator': 'equals'
           }]
          }

with open(NAMES_FILE, 'r') as file:
    # Read the provided list of species names
    names = [name.strip('\n') for name in file.readlines()[1:]]

with open(OUTPUT_FILE, 'w') as file:
    # This will create a file if it doesn't exist already
    file.write('[') 
    for name in names:
        params['textCriteria'][0]['searchToken'] = name

        data = requests.post(url=ENDPOINT, json=params)
        data.raise_for_status()
        
	json.dump(data.json(), file, indent=2)

        file.write(',')
    file.write(']')
   # json.dump() can also write an array to file, so it would be valid to save all the search results in a list and then dump. I chose to add brackets and commas with file.write() instead because, when something went wrong during the loop, I could restart from the middle without losing the earlier successful searches. 
