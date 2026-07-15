#!/usr/bin/env python3
"""Test that curl_cffi can defeat Cloudflare on OSCN."""

from curl_cffi import requests

url = ("https://www.oscn.net/applications/oscn/report.asp"
       "?report=WebJudicialDocketCaseTypeAll&errorcheck=true"
       "&database=&db=Comanche&CaseTypeID=31&StartDate=2022-10-17"
       "&GeneralNumber=1&generalnumber1=1")

# impersonate Safari TLS fingerprint + headers
resp = requests.get(url, impersonate="edge101", timeout=30)

print(f"Status: {resp.status_code}")
print(f"Body length: {len(resp.text)}")
print(f"Contains Turnstile: {'Turnstile' in resp.text or 'cf-turnstile' in resp.text}")
print(f"Contains case info: {'GetCaseInformation' in resp.text}")

with open("data/diagnostic/python_test_comanche.html", "w") as f:
    f.write(resp.text)

print("\nSaved to data/diagnostic/python_test_comanche.html")
