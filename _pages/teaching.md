---
layout: page
permalink: /teaching/
title: Teaching
description: Teaching assistant at <a href='https://www.universite-paris-saclay.fr/en' target='_blank'>Université Paris-Saclay</a> and <a href='https://www.ensta.fr/' target='_blank'>ENSTA Paris</a>.
nav: true
nav_order: 3
---

{% assign root = "https://www.imo.universite-paris-saclay.fr/~guillaume.principato/" %}

## Université Paris-Saclay

Multidimensional exploratory analysis (L3 level), 2023, 2024 and 2026

Inferential statistics and data analysis (L3 level), 2023

Statistics for biology (L2 level), 2025 and 2026

## ENSTA Paris

Time series (M1 level), 2024 and 2025

Material for this course can be fund on <a href='https://www.imo.universite-paris-saclay.fr/~yannig.goude/about.html' target='_blank'>Yannig Goude website</a>. You can also find my own correction.

### STA202 - Practical Sessions

<ul>
{% assign base_path = "assets/teaching/STA202" %}
{% for session in (1..6) %} 
  {% assign folder = base_path | append: "/Practical_session_" | append: session %}
  <li>
    Practical session {{ session }}:
    <a href="{{ root }}{{ folder }}/TP{{ session }}_ennonce.pdf">instruction</a>,
    <a href="{{ root }}{{ folder }}/TP{{ session }}_prefilled.R" download>pre-filled</a>,
    <a href="{{ root }}{{ folder }}/TP{{ session }}_corrected.R" download>correction</a>
    {% if session == 1 or session == 5 or session == 6 %}
      , <a href="{{ root }}{{ folder }}/TP{{ session }}_data.zip">data</a>
    {% endif %}
  </li>
{% endfor %}
</ul>