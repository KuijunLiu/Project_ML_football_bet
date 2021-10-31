from selenium import webdriver
from selenium.webdriver.common.action_chains import ActionChains
import time
import json
import os

options = webdriver.ChromeOptions()
options.add_argument('headless')
driver = webdriver.Chrome(executable_path="chromedriver.exe", chrome_options=options)

home_url = "https://www.oddsportal.com/soccer/england/premier-league/results/"
driver.get(home_url)
time.sleep(2)
print("Navigated to the home page")


def get_list_pages():
    max_page = 2
    list_pages = []
    while True:
        try:
            list_pages.append(driver.find_element_by_xpath(f'//*[@id="pagination"]/a[{max_page}]'))
            txt = list_pages[-1].text
            max_page += 1
            if max_page == 1000:
                print('ERROR IN GETTING THE PAGE ELEMENTS')
                break
        except:
            list_pages = list_pages[:-2]
            break
    return list_pages

def get_list_matches():
    list_matches = driver.find_elements_by_partial_link_text('-')[1:]
    return list_matches

def read_odds(num=4):
    this_data = {}
    odd_lines = driver.find_elements_by_class_name('odd')
    odd_lines.extend(driver.find_elements_by_class_name('even'))
    odd_values = [x.text.split("  ") for x in odd_lines]
#     print(odd_values)
    for o in odd_values:
        if len(o) == 2 and len(o[-1].split("\n")[1:]) == num:
            if o[0].replace(" ", "") not in this_data.keys():
                this_data[o[0].replace(" ", "")] = [o[-1].split("\n")[1:]]
            else:
                this_data[o[0].replace(" ", "")].append(o[-1].split("\n")[1:])
        else:
            continue
    return this_data

def read_odds_type2(num=4):
    this_data = {}
    pos = driver.find_elements_by_partial_link_text("Compare odds")
    for p in pos:
        p.click()
        time.sleep(0.1)
    this_AH = read_odds(num=num)
    for k in this_AH:
        if k not in this_data.keys():
            this_data[k] = [this_AH[k]]
        else:
            this_data[k].append(this_AH[k])
#     p.click()
    time.sleep(0.1)
    return this_data

def fetch_match_data():
    this_data = {}
    this_data["1x2"] = read_odds()
    fh = driver.find_element_by_link_text("1st Half")
    fh.click()
    time.sleep(1)
    this_data['1x2fh'] = read_odds()
    sh = driver.find_element_by_link_text("2nd Half")
    sh.click()
    time.sleep(1)
    this_data['1x2sh'] = read_odds()
    try:
#         mozz
        AH = driver.find_element_by_link_text("AH")
        AH.click()
        time.sleep(1)
        this_data['AH'] = read_odds_type2()
        fh = driver.find_element_by_link_text("1st Half")
        fh.click()
        time.sleep(1)
        this_data['AHfh'] = read_odds_type2()
        sh = driver.find_element_by_link_text("2nd Half")
        sh.click()
        time.sleep(1)
        this_data['AHsh'] = read_odds_type2()
    except:
        print("no asian handicap for this match")
    try:
#         mozz
        OU = driver.find_element_by_link_text("O/U")
        OU.click()
        time.sleep(1)
        this_data['OU'] = read_odds_type2()
        fh = driver.find_element_by_link_text("1st Half")
        fh.click()
        time.sleep(1)
        this_data['OUfh'] = read_odds_type2()
        sh = driver.find_element_by_link_text("2nd Half")
        sh.click()
        time.sleep(1)
        this_data['OUsh'] = read_odds_type2()
    except:
        print("no over/under for this match")
    try:
#         mozz
        EH = driver.find_element_by_link_text("EH")
        EH.click()
        time.sleep(1)
        this_data['EH'] = read_odds_type2(num=5)
        fh = driver.find_element_by_link_text("1st Half")
        fh.click()
        time.sleep(1)
        this_data['EHfh'] = read_odds_type2(num=5)
        sh = driver.find_element_by_link_text("2nd Half")
        sh.click()
        time.sleep(1)
        this_data['EHsh'] = read_odds_type2(num=5)
    except:
        print("no over/under for this match")
    more_bets = "More bets"
    try:
#         mozz
        mb = driver.find_element_by_partial_link_text(more_bets)
        ActionChains(driver).move_to_element(mb).perform()
        OE = driver.find_element_by_partial_link_text("Odd or Even")
        OE.click()
        time.sleep(1)
        this_data['OE'] = read_odds(num=3)
        fh = driver.find_element_by_link_text("1st Half")
        fh.click()
        time.sleep(1)
        this_data['OEfh'] = read_odds(num=3)
        sh = driver.find_element_by_link_text("2nd Half")
        sh.click()
        time.sleep(1)
        this_data['OEsh'] = read_odds(num=3)
        more_bets = "Odd or Even"
    except:
        print("no odd/even for this match")
    try:
#         mozz
        mb = driver.find_element_by_partial_link_text(more_bets)
        ActionChains(driver).move_to_element(mb).perform()
        BTS = driver.find_element_by_partial_link_text("Both Teams to Score")
        BTS.click()
        time.sleep(1)
        this_data['BTS'] = read_odds(num=3)
        fh = driver.find_element_by_link_text("1st Half")
        fh.click()
        time.sleep(1)
        this_data['BTSfh'] = read_odds(num=3)
        sh = driver.find_element_by_link_text("2nd Half")
        sh.click()
        time.sleep(1)
        this_data['BTSsh'] = read_odds(num=3)
        more_bets = "Both Teams to Score"
    except:
        print("no both_team_to_score for this match")
    try:
#         mozz
        mb = driver.find_element_by_partial_link_text(more_bets)
        ActionChains(driver).move_to_element(mb).perform()
        DC = driver.find_element_by_partial_link_text("Double Chance")
        DC.click()
        time.sleep(1)
        this_data['DC'] = read_odds()
        fh = driver.find_element_by_link_text("1st Half")
        fh.click()
        time.sleep(1)
        this_data['DCfh'] = read_odds()
        sh = driver.find_element_by_link_text("2nd Half")
        sh.click()
        time.sleep(1)
        this_data['DCsh'] = read_odds()
        more_bets = "Double Chance"
    except:
        print("no double_chance for this match")
    
    try:
#         mozz
        mb = driver.find_element_by_partial_link_text(more_bets)
        ActionChains(driver).move_to_element(mb).perform()
        HF = driver.find_element_by_partial_link_text("Half Time / Full Time")
        HF.click()
        time.sleep(1)
        this_data['HF'] = read_odds_type2(1)
        more_bets = "Half Time / Full Time"
    except:
        print("no half_time_full_time for this match")
        
        
    return this_data


list_seasons = driver.find_elements_by_partial_link_text('/')
print("number of seasons in this page: " , len(list_seasons))
num_data = len(os.listdir("data_crawl"))
ct = 0
for id_s in range(len(list_seasons)):
    list_seasons = driver.find_elements_by_partial_link_text('/')
    this_season_text = list_seasons[id_s].text
    print("STARTING FETCHING SEASON: ", this_season_text)
    list_seasons[id_s].click()
    time.sleep(2)
    list_pages = get_list_pages()
    print("number of pages: " , len(list_pages))
    for id_p in range(len(list_pages)):
        list_pages = get_list_pages()
        list_pages[id_p].click()
        time.sleep(2)
        list_matches = get_list_matches()
        print("number of matches in this page: " , len(list_matches))
        for id_m in range(len(list_matches)):
            if ct >= num_data:
                list_matches = get_list_matches()
    #             print([x.text for x in list_matches])
                this_match_text = list_matches[id_m].text 
                print("Match: ", this_match_text)
                list_matches[id_m].click()
                time.sleep(2)
                this_match_data = fetch_match_data()
                this_match_data["season"] = this_season_text
                this_match_data["match"] = this_match_text
    #             list_data.append(this_match_data)
                with open(f'data_crawl/data{ct}.json', 'w') as f:
                    json.dump(this_match_data, f)
                    ct += 1
                    f.close()

                driver.get(home_url)
                time.sleep(2)
                list_seasons = driver.find_elements_by_partial_link_text('/')
                list_seasons[id_s].click()
                time.sleep(2)
                list_pages = get_list_pages()
                list_pages[id_p].click()
                time.sleep(2)
            else:
                ct += 1
                
            