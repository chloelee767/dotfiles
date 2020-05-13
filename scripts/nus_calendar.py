"""
A script to generate an calendar (.ics) file with all the weeks (Week 1 - Exam Week 2) for a semester in NUS.
Each week runs from Monday to Sunday.

Sample usage:
s = make_ics_for_sem(dt.datetime(2020,1,13))
open('ay1920sem2_weeks.ics','w').write(s)
"""

import datetime as dt

def to_date_string(date):
    return date.strftime('%Y%m%d')

def make_week(first_day,name):
    start_str = to_date_string(first_day)
    end_str = to_date_string(first_day + dt.timedelta(days=7)) # end is non inclusive
    # TRANSPARENT - sets availability to free in Google calendar
    return 'BEGIN:VEVENT\n' + \
        f'DTSTART;VALUE=DATE:{start_str}\nDTEND;VALUE=DATE:{end_str}\n'+ \
        f'SUMMARY:{name}\nSTATUS:CONFIRMED\n' + \
        'TRANSP:TRANSPARENT\n' + \
        'END:VEVENT\n'

def make_week_names():
    names = []
    for i in range(1,14):
        names.append(f'Week {i}')
    names[6:6] = ['Recess Week'] # insert recess week between week 6 and 7
    names.append('Reading Week')
    for i in range(1,3):
        names.append(f'Exam Week {i}')
    return names

def make_ics_for_sem(first_day_of_sem):
    """This is the main function which generates the contents of the ics file."""

    current = first_day_of_sem
    num_weeks = 13 + 1 + 2 # 13 weeks + recess week + exam

    string = 'BEGIN:VCALENDAR\n'
    for week in make_week_names():
        print(current)
        string += make_week(current, week)
        current = current + dt.timedelta(days=7)
    string += 'END:VCALENDAR'

    return string

s = make_ics_for_sem(dt.datetime(2020,8,10))
open('ay2021sem1_weeks.ics','w').write(s)

# import sys
# if len(sys.argv) < 2:
#     print("")
# else:
#     firstday = sys.argv[1]
#     out = sys.argv[2] if len(sys.argv) > 3 else "nus-calendar-" + str(firstday)

#     # firstday =
