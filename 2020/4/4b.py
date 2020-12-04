import re
def parse_to_doc(string):
  doc = {}
  fields = string.split()
  for field in fields:
    key,val = field.split(":")
    doc[key] = val
  return doc

required_fields = ['byr','iyr','eyr','hgt','hcl','ecl','pid'] 

def is_passport(doc):
  for req in required_fields:
    if not req in doc:
      return 0
  return 1

"""
    byr (Birth Year) - four digits; at least 1920 and at most 2002.
    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    hgt (Height) - a number followed by either cm or in:
        If cm, the number must be at least 150 and at most 193.
        If in, the number must be at least 59 and at most 76.
    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    pid (Passport ID) - a nine-digit number, including leading zeroes.
    cid (Country ID) - ignored, missing or not.
"""
valid_ecl = ['amb','blu','brn','gry','grn','hzl','oth']
validator = {
  'byr' : lambda x : 1920 <= int(x) <= 2002, 
  'iyr' : lambda x : 2010 <= int(x) <= 2020,
  'eyr' : lambda x : 2020 <= int(x) <= 2030,
  'hgt' : lambda x : ( 150 <= int(x[:-2]) <= 193  if x[-2:] == "cm" 
                  else  59 <= int(x[:-2]) <= 76   if x[-2:] == "in" else False ),
  'hcl' : lambda x : True if re.match(r'(^#[0-9a-f]{6}$)', x) else False,
  'ecl' : lambda x : x in valid_ecl ,
  'pid' : lambda x : (True if int(x) != None else False) if len(x) == 9 else False,
  'cid' : lambda x : True,
}
def is_valid(pp):
  for key, val in pp.items():
    try:
      if not validator[key](val):
        return False
    except:
     return False
  return True

with open("input", "r") as f:
  rawdocs = f.read().split('\n\n')
  docs = [parse_to_doc(rawdoc) for rawdoc in rawdocs]
  passports = [doc for doc in docs if is_passport(doc)]
  print(len([passport for passport in passports if is_valid(passport)]))
