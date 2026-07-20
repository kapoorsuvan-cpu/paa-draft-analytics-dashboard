# Model notes

Box-score statistics alone have a ceiling for NFL draft-round prediction.
The highest-leverage future additions are:

- age on draft day;
- games started and snap counts;
- combine/pro-day measurements;
- pressure, coverage, and blocking data;
- injury and availability history;
- awards and all-conference recognition.

The v9 pipeline avoids pretending that missing data are average data by adding
coverage and missingness indicators and removing sparse features when sufficient
alternatives exist.

Offensive-line models remain the weakest because CFBD player box scores do not
contain blocking performance. Recruiting, size, school history, and context are
used as fallbacks, but external OL data would materially improve accuracy.
