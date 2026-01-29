---
title: Research
permalink: /research/
layout: page
nav: true
nav_order: 1


---


## Preprints

<div class="publications-grid">
{% assign preprints = site.publications
  | where_exp:"item","item.type == 'preprint'"
  | sort: "year"
  | reverse %}
{% for pub in preprints %}
  {% include publication_card.html pub=pub %}
{% endfor %}
</div>

## Publications

<div class="publications-grid">
{% assign pubs = site.publications
  | where_exp:"item","item.type == 'publication'"
  | sort: "year"
  | reverse %}
{% for pub in pubs %}
  {% include publication_card.html pub=pub %}
{% endfor %}
</div>