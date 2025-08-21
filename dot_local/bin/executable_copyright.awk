#!/usr/bin/gawk -f
# Copyright (C) 2013 Ryan Kavanagh <rak@debian.org>
# Given a series of lines in the format
#  Copyright (c) NNNN, MMMM-MMMM, ..., NNNN John Smith <jsmith@example.org>
# group years and emails by person.

{
    match($0, /.*Copyright.*[0-9][,]? +/);
    DATE_LENGTH = RLENGTH;
    match($0, /<.*>/);
    EMAIL_START = RSTART;
    if (RLENGTH != -1) {
        NAME = substr($0, DATE_LENGTH + 1, EMAIL_START - DATE_LENGTH - 2);
        EMAIL = substr($0, EMAIL_START);
    } else {
        # No email on this line
        NAME = substr($0, DATE_LENGTH + 1);
    }
    match($0, /.*Copyright +\([cC]\) +/);
    DATE_START = RLENGTH + 1;
    YEARS = substr($0, DATE_START, DATE_LENGTH - DATE_START);
    gsub(/, +/, " ", YEARS);
    gsub(/,/, " ", YEARS);
    people_years[NAME] = people_years[NAME] " " YEARS;
    if (EMAIL_LENGTH != -1) {
        email_pattern = "/.*" EMAIL ".*/";
        if (!(NAME in people_emails)) {
            people_emails[NAME] = EMAIL;
        } else if (!match(people_emails[NAME], EMAIL)) {
            people_emails[NAME] = people_emails[NAME] "," EMAIL;
        }
    }
} END {
    for (person in people_years) {
        delete years_array;
        split(people_years[person], years_array); 
        # Split any hyphenated years;
        for (year in years_array) {
            if (years_array[year] ~ /[0-9]+-[0-9]+/) {
                delete split_year;
                split(years_array[year], split_year, /-/);
                years_array[year] = split_year[1];
                if (split_year[1] != split_year[2]) {
                    # Make sure it isn't some crappy input like 2012-2012
                    for (j = 1; j <= split_year[2] - split_year[1]; j++) {
                        years_array[length(years_array) + 1] = \
                                   years_array[year] + j;
                    }
                }
            }
        }
        # Sort the years
        asort(years_array);
        # Delete any duplicates:
        for (i = 1; i <= length(years_array); i++) {
            if (i > 1 && years_array[i-1] == years_array[i]) {
                # Delete years_array[i-1] instead of years_array[i] so that we
                # can still check the next year with ease
                delete years_array[i-1];
            }
        }
        # Final sort
        asort(years_array);
        # Remove duplicates and generate year string
        year_string = "";
        # Force AWK to access the years in order
        added_hyphen = 0;
        for (i = 1; i <= length(years_array); i++) {
            if (i > 1) {
                if (years_array[i - 1] != years_array[i]) {
                    # added_hyphen tracks if the last character in the string is
                    # a hyphen
                    if ((!added_hyphen) && (years_array[i - 1] == years_array[i] - 1)) {
                        # year_string isn't terminated by a hyphen, and the year
                        # at i-1 is one less than the current one
                        year_string = year_string "-";
                        added_hyphen = 1;
                    } else if (added_hyphen && (years_array[i - 1] != years_array[i] - 1)) {
                        # The string is terminated by a hyphen, but the current
                        # year does not immediately follow the preceeding
                        # one
                        year_string = year_string years_array[i-1] ", " years_array[i];
                        added_hyphen = 0;
                    } else if (!added_hyphen) {
                        year_string = year_string ", " years_array[i];
                    }
                }
            } else {
                year_string = years_array[i];
            }
        }
        # We've added a hyphen, but run out of years to check, terminate it
        if (added_hyphen) {
            year_string = year_string years_array[length(years_array)];
        }
        final_line[years_array[length(years_array)]][length(years_array)][person] = \
            "Copyright (C) " year_string "\t" person " " people_emails[person];
    }
    # We can't sort the years indices with asorti because we want a numerical,
    # not lexicographic sort of the indices.
    j = 0;
    delete years_sorted;
    for (i in final_line) years_sorted[j++] = i+0;
    n_years_entries = asort(years_sorted);
    # And output the lines with the most recent contributor first
    for (y = n_years_entries; y >= 1; y--) {
        # Sort the contributors with most recent contribution in year
        # by_year[y] by number of years contributed:
        j = 0;
        delete contributions_sorted;
        for (i in final_line[years_sorted[y]]) contributions_sorted[j++] = i+0;
        n_contrib_entries = asort(contributions_sorted);
        for (c = n_contrib_entries; c >= 1; c--) {
            # Finally, sort by contributor name
            asorti(final_line[years_sorted[y]][contributions_sorted[c]], by_person);
            # And output the lines in alphabetical order by person name
            for (n = 1; n <= length(by_person); n++) {
                print final_line[years_sorted[y]][contributions_sorted[c]][by_person[n]];
            }
        }
    }
}
