
// assign initial values to agents
List<SamplingElement> incomeTypeList = new ArrayList<>();

double proportion = (1.0/3.0);
incomeTypeList.add(new SamplingElement(proportion, "Type1"));
incomeTypeList.add(new SamplingElement(proportion, "Type2"));
incomeTypeList.add(new SamplingElement(proportion, "Type3"));

RandomSampler randomSampler = new RandomSampler(incomeTypeList);

for (County c : counties) {
	for (Person p : c.people) {
		p.p_incomeType = randomSampler.getRandom().name;
		f_assignIncome(p);
		p.p_incomeMortalityRatio = v_incomeMortalityRatios.get(p.p_incomeType);
		v_statsIncome.addValue(p.p_income);
		c.v_statsIncome.addValue(p.p_income);
	}
}