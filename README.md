# Life Insurance Pricing in R

## Project Overview

This project was developed as part of a Life Insurance course at the University of Groningen.

The aim was to model and evaluate two life insurance products using actuarial methods and Swiss life tables:

- a pure endowment product with monthly premium payments
- a joint life annuity for a couple approaching retirement

The analysis focuses on actuarial present values, survival probabilities, mortality assumptions, discounting and variance calculations.

## Main Tasks

The project included:

- importing and preparing Swiss male and female life tables
- calculating survival and mortality probabilities
- applying discount factors to future cash flows
- determining the monthly premium required to reach a target retirement amount
- calculating the actuarial present value of a joint life annuity
- estimating the variance and standard deviation of the modeled cash flows
- handling fractional ages under a uniform distribution of deaths assumption

## Models

### Pure Endowment

The first part of the project models a pure endowment contract.

The goal is to determine the monthly premium required for the policyholder to accumulate CHF 200,000 by retirement, taking survival probabilities and discounting into account.

The model also calculates the variance of the contract value.

### Joint Life Annuity

The second part models a geometrically increasing joint life annuity for a retiring couple.

Payments:

- start at retirement
- are made three times per year
- increase by 1% annually
- continue until the first death of the couple

The model calculates the actuarial present value and variance of the annuity using male and female Swiss life tables.
