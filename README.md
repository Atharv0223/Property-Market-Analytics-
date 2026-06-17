Property Market Analytics at Scale
Distributed real-estate price forecasting and real-time market analytics using Apache Spark, Spark MLlib, and Kafka.

Overview
Property Market Analytics at Scale is a big-data analytics platform built to process and forecast Australian real-estate market trends using distributed computing technologies. The project leverages Apache Spark for large-scale data processing, Spark MLlib for machine learning model development, and Apache Kafka for real-time streaming inference.

The system ingests property transaction data, engineers predictive features, trains forecasting models, and deploys them through a real-time streaming pipeline capable of generating live market predictions.

Key Outcomes
Processed large-scale Australian property transaction datasets using distributed Spark workflows.
Built and optimized Gradient Boosted Tree and Random Forest forecasting models.
Achieved approximately 15% RMSLE reduction compared to baseline forecasting approaches.
Operationalized predictions using a Kafka + Spark Structured Streaming architecture.
Enabled near real-time property price forecasting and market trend monitoring.

Architecture

                    ┌────────────────────┐
                    │ Property Datasets  │
                    │ Historical Sales   │
                    └──────────┬─────────┘
                               │
                               ▼
                    ┌────────────────────┐
                    │ Apache Spark       │
                    │ Data Processing    │
                    │ Feature Engineering│
                    └──────────┬─────────┘
                               │
                               ▼
                    ┌────────────────────┐
                    │ Spark MLlib        │
                    │ Model Training     │
                    │ RF & GBT Models    │
                    └──────────┬─────────┘
                               │
                               ▼
                    ┌────────────────────┐
                    │ Model Evaluation   │
                    │ RMSLE Optimization │
                    └──────────┬─────────┘
                               │
                               ▼
          ┌─────────────────────────────────────┐
          │ Kafka + Spark Structured Streaming  │
          │ Real-Time Prediction Pipeline       │
          └─────────────────────────────────────┘
                               │
                               ▼
                    ┌────────────────────┐
                    │ Live Forecasts     │
                    │ Market Insights    │
                    └────────────────────┘



| Component           | Technology                 |
| ------------------- | -------------------------- |
| Data Processing     | Apache Spark               |
| Distributed Queries | Spark SQL                  |
| Machine Learning    | Spark MLlib                |
| Streaming           | Spark Structured Streaming |
| Message Broker      | Apache Kafka               |
| Language            | Python (PySpark)           |
| Storage             | CSV / Parquet / Data Lake  |
| Evaluation Metric   | RMSLE                      |


Dataset
The project uses Australian real-estate transaction data containing attributes such as:

Property sale price
Property type
Location and suburb information
Land size
Building size
Number of bedrooms
Number of bathrooms
Sale date
Market indicators
Data Processing Pipeline
Data ingestion from raw transaction datasets.
Missing value handling and cleaning.
Feature engineering and categorical encoding.
Distributed transformations using Spark SQL.
Dataset splitting for training and testing.
Model training and hyperparameter tuning.


Machine Learning Models
Random Forest Regressor
Used as a robust ensemble baseline for handling non-linear relationships in housing data.

Advantages
Handles feature interactions effectively.
Resistant to overfitting.
Works well on large distributed datasets.

Gradient Boosted Trees (GBT)
Used as the primary forecasting model due to stronger predictive performance.

Advantages
Captures complex market patterns.
Higher predictive accuracy.
Improved handling of heterogeneous property features.

Real-Time Streaming Pipeline

The trained forecasting model was deployed using Kafka and Spark Structured Streaming.

Streaming Workflow:
Property Events
       │
       ▼
 Apache Kafka Topic
       │
       ▼
Spark Structured Streaming
       │
       ▼
Feature Transformation
       │
       ▼
Model Inference
       │
       ▼
Prediction Output
       │
       ▼
Dashboard / Consumer


Project Structure:
property-market-analytics/
│
├── data/
│   ├── raw/
│   └── processed/
│
├── notebooks/
│   ├── data_exploration.ipynb
│   ├── feature_engineering.ipynb
│   └── model_training.ipynb
│
├── src/
│   ├── preprocessing/
│   ├── feature_engineering/
│   ├── models/
│   ├── streaming/
│   └── evaluation/
│
├── configs/
│
├── models/
│
├── kafka/
│
├── outputs/
│
└── README.md

Running the Project
Prerequisites
Python 3.10+
Apache Spark 3.x
Apache Kafka
Java 11+
PySpark


Business Impact

This platform demonstrates how distributed machine learning and streaming analytics can support:

Property valuation forecasting
Real-estate investment analysis
Market trend detection
Risk assessment
Real-time decision support

The solution combines scalable big-data processing with production-oriented deployment practices, enabling property market insights at enterprise scale.

Skills Demonstrated
Apache Spark
PySpark
Spark SQL
Spark MLlib
Distributed Computing
Machine Learning
Feature Engineering
Hyperparameter Tuning
Apache Kafka
Structured Streaming
Real-Time Analytics
Model Deployment
Big Data Engineering
