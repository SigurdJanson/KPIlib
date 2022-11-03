# The Data Format of KPI Kluster

Each data set can have these pieces of information:

```json
,
    {
        "title": "",
        "description": "",
        "interpretation": "",
        "formula": "",
        "direction": "",
        "unit": "",
        "rating_count": 0,
        "rating_avg": 0,
        "framework": "",
        "tags": "",
        "domain": "",
        "created_at": "2022-10-12 00:00:00",
        "updated_at": "2022-10-12 00:00:00",
        "url": "",
        "id": 3000000,
        "intervention_needed": false
    }
```

* The `title` is the name of the KPI.
* The `description` provides a brief explanation of what the KPI is and what it does. The more self-explanatory the title is, the shorter the description may be. 
  * HTML tags are possible but will be stripped away in the tile view if the text is too long and it is cut off. The table view and the details dialogue both show links.
* `id` is a unique identifier. Ids starting with a 9 are sourced from the ResearchNow platform. A starting 6 indicates a KPI added by the author that is not related to digital product development/e-business. A 3 is again author-defined but relevant for digital product development/e-business.
* `formula` can be displayed as mathematical formula if it begins and ends with the two formula-defining characters `$$`.
* The `interpretation` provides explanations how the indicator works, benchmarks for them, or any caveats to be aware off.
* `unit` is one of "Area", "Data", "Distance", "Energy", "Money", "Number", "Percentage", "Ratio", "Score", "Time", "Volume", "Weight".
  * A **ratio** is any fraction with different units involved. Any speed is considered a ratio (e.g. MB/s or km/s).
  * A ratio can be interpreted as **percentage** when the units in nominator and denominator are identical.
  * A **Number** is a metric that is counted (even when used as average).
* `direction` can be "Maximize", "Minimize", or "Range" (or "\[None]" if the KPI has no generally valid direction or it has not been specified, yet).
* `domain` is a business domain for which the KPI is useful for.
* `framework` can be one of those:
  * **401k** is an employer-sponsored, defined-contribution, personal pension (savings) account in the United States.
  * **Alignability**. The Alignability® Process Model provides field-proven IT Service Management processes for the BMC Service Desk Express application suite. The Alignability® processes are based on a combination of the best-practice ITIL® methodology and the Total Quality Management principle of continuous improvement.
  * **ASL**: Application Service Library (ASL)
  * **BiSL**: The "Business Information Services Library" (BiSL) provides a public domain standard approach for business information management.
  * **BMF**
  * [**Cobit**](https://en.wikipedia.org/wiki/COBIT) (Control Objectives for Information and Related Technologies) is a framework created by ISACA for information technology (IT) management and IT governance. It is business focused and defines a set of generic processes for the management of IT, with each process defined together with process inputs and outputs, key process-activities, process objectives, performance measures and an elementary maturity model.
  * **HEART**: The HEART framework by Google.
  * **ITIL**: The Information Technology Infrastructure Library (ITIL) is a set of detailed practices for IT activities such as IT service management (ITSM) and IT asset management (ITAM) that focus on aligning IT services with the needs of business.
  * **MOF** refers to the [Microsoft Operations Framework](https://en.wikipedia.org/wiki/Microsoft_Operations_Framework).
  * **PMBOK** 
  * **SAFe** (Scaled Agile Framework Enterprise) "is a set of organization and workflow patterns intended to guide enterprises in scaling lean and agile practices" (Wikipedia).
  * **Six Sigma**: [Six Sigma is a set of techniques and tools for process improvement.](https://en.wikipedia.org/wiki/Six_Sigma).
  * **SOX**: Compliance with the Sarbanes-Oxley Act, a United States federal law that mandates certain practices in financial record keeping and reporting for corporations.
  * **SCOR** refers the the "Supply Chain Operations Reference Model (SCOR)" developed by Supply Chain Council (SCC). The SCC is an independent not-for-profit organisation with the objective to develop a standard supply-chain process reference model enabling effective communication among the supply chain partners.
  * **UBPR**
  * **VRM** ????https://en.wikipedia.org/wiki/Vendor_relationship_management
* A link providing further information.


# Future Attributes

These attributes need to be decided:

1. Dimension: Reliability, Velocity, Adaptability, Cost, Asset, Innovation, Customer
2. Control Type - preventive, reactive
3. Dpt.: IT, Manufacturing, Marketing, R&D, Product dev., ... **TBD**
4. Industry: Internet, 



