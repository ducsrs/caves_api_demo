import json
import requests
import csv

statuses = {
    'X': 'Presumed Extinct',
    'H': 'Possibly Extinct',
    '1': 'Critically Imperiled',
    '2': 'Imperiled',
    '3': 'Vulnerable',
    '4': 'Apparently Secure',
    '5': 'Secure',
    'U': 'Unrankable',
    'NR': 'Unranked',
    'NA': 'Not Applicable'
}

def first_ten(fields, result, taxon_result):
    # appends every non-regional field
    species_global = result['speciesGlobal']
    # nested dict that contains some of what we want
    fields['Species'].append(result['scientificName'])
    fields['Common Name'].append(result['primaryCommonName']
                                 if result['primaryCommonName'] else '')
    fields['Kingdom'].append(species_global['kingdom'])
    fields['Phylum'].append(species_global['phylum'])
    fields['Class'].append(species_global['taxclass'])
    fields['Order'].append(species_global['taxorder'])
    fields['Family'].append(species_global['family'])
    fields['Global Status'].append(result['roundedGRank'] + ': ' + statuses[result['roundedGRank'][1:]])
    fields['Range Extent'].append('"' + taxon_result['rankInfo']['rangeExtent']['rangeExtentDescEn'] + '"'
                                  if taxon_result['rankInfo']['rangeExtent'] else '')
    fields['USESA Code'].append(species_global['usesaCode']
                                if species_global['usesaCode'] else '')

def make_csv(in_file, out_file):
    with open('taxons.json', 'r') as file:
        taxons = json.load(file)
    with open(in_file, 'r') as file:
        data = json.load(file)

    fields = {'Species': [],
              'Common Name': [],
              'Kingdom': [],
              'Phylum': [],
              'Class': [],
              'Order': [],
              'Family': [],
              'Global Status': [],
              'Range Extent': [],
              'USESA Code': [],
              'Region': [],
              'Regional Status': []}
    for result, taxon in zip(data, taxons):
        # taxon_result = requests.get(f'https://explorer.natureserve.org/api/data/taxon/{result["uniqueId"]}').json()
        # taxons.append(taxon_result)

        for nation in result['nations']:
            if nation['subnations']:
                for subnation in nation['subnations']:
                    first_ten(fields, result, taxon)
                    fields['Region'].append(subnation['subnationCode'])
                    fields['Regional Status'].append(subnation['roundedSRank'] + ': ' + statuses[subnation['roundedSRank'][1:]])
            else:
                first_ten(fields, result, taxon)
                fields['Region'].append("")
                fields['Regional Status'].append("")

    with open('taxons.json', 'w') as file:
        json.dump(taxons, file, indent=2)
        # save taxon results in case we want more from them

    with open(out_file, 'w') as file:
        file.write(','.join(fields.keys()))
        file.write('\n')
        # write the header
        for line in zip(*fields.values()):
            # write the nth entry of each list
            file.write(','.join(line))
            file.write('\n')


make_csv('june_results.json', 'tax_region_range_usesa.csv')
