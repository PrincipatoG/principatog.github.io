---
layout: page
permalink: /teaching/
title: Teaching
description: Teaching assistant at <a href='https://www.universite-paris-saclay.fr/en' target='_blank'>Université Paris-Saclay</a> and <a href='https://www.ensta.fr/' target='_blank'>ENSTA Paris</a>.
nav: true
nav_order: 3
---

## Université Paris-Saclay

Multidimensional exploratory analysis (L3 level), 2023 and 2024

Inferential statistics and data analysis (L3 level), 2023

Statistics for biology (L2 level), 2025

## ENSTA Paris

Time series (M1 level), 2024 and 2025

Material for this course can be fund on <a href='https://www.imo.universite-paris-saclay.fr/~yannig.goude/about.html' target='_blank'>Yannig Goude website</a>. You can also find my own correction, based on <a href='https://mzaffran.github.io/' target='_blank'>Margaux</a>'s, for which I thank her.

### STA202 - Practical Sessions

<ul>
{% assign base_path = "assets/teaching/STA202" %}
{% for session in (1..6) %} 
  {% assign folder = base_path | append: "/Practical session " | append: session %}
  <li>
    Practical session {{ session }}: 
    <a href="{{ folder }}/TP{{ session }}_to_fill.R">instruction</a>, 
    <a href="{{ folder }}/TP{{ session }}_corrected.R">correction</a>
  </li>
{% endfor %}
</ul>