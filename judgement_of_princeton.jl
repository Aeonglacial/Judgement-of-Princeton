### A Pluto.jl notebook ###
# v0.12.7

using Markdown
using InteractiveUtils

# ╔═╡ fc493a80-0cba-11eb-257b-bdd6b4439dd1
begin
	using HTTP
	using CSV, DataFrames
	using StatsBase, StatsFuns, Distributions
	using Turing
	using StatsPlots
end

# ╔═╡ ffd2bd20-0cbf-11eb-3f27-61c959ab7310
md"# The Judgment of Princeton"

# ╔═╡ da0002a0-0cc0-11eb-2ad6-8d91c6735423
md"Modeled after the Famous 1976 blind tasting, The Judgement of Paris - the event in which kicked off the Californian Wine industry - The Judgement of Princeton was a blind tasting held on June 8, 2012 during a conference of the American Association of Wine Economists. Hosted by Princeton University - The purpose of this event was to compare, by a blind tasting, of several French wines against wines produced in New Jersey in order to gauge the quality and development of the New Jersey wine industry."

# ╔═╡ b1f22000-0cd3-11eb-3439-470d4fcb1f4f
md"This notebook attempts to explore potential bias amongst French & American judges. Because American wine is relatively new to the industry, namely New Jersey wine, I am wondering if there is a noticeable difference in taste...especially if you are used to drinking old world, French wines.

Here are the judges:"

# ╔═╡ f0bfd320-1ab8-11eb-3166-ef0113e3289b
md"
**John Foy** - A former tax consultant who got into wine & food after living in Brussels

**Linda Murphy** - Former sports journalist turned wine journalist and lecturer

**Francis Schott** - New Jersey local, co-founder and beverage director of Catherine Lombardi Restaurant, Stage Left Steak and StageLeftWineShop.com

**Tyler Colman** - Aka Dr. Vino, a phD in political science and published wine authur

**Robert Hodgson** - Retired statistics professor, released a study which showed that wine judges at the California State Fair gave dramatically different scores to the same wine when tasting it blind on two different occasions

**Jamal Rayyis** - Freelance wine and food writer + lecturer, with an eye toward culture, politics, and deconstructing 'tradition' 

**Jean-Marie Cardebat** - Professor of Economics, Université de Bordeaux

**Olivier Gergaud** - Professor of Economics at KEDGE Business School

**Danièle Meulders** - Professor of Economics, Université Libre de Bruxelles
"

# ╔═╡ 798cb770-0cd3-11eb-1e8f-a7710269a913
md"Libraries we are using for analyzing this dataset are:"

# ╔═╡ 93e9e8d0-0cbb-11eb-09b9-3bb9799867b8
md"First lets begin by downloading the data from a git repository then storing the data to memory"

# ╔═╡ b72c6c80-0e70-11eb-39f0-87f276855ce6
begin
	url = "https://raw.githubusercontent.com/rmcelreath/rethinking/master/data/Wines2012.csv"
	resp = HTTP.get(url)
	data = CSV.read(IOBuffer(resp.body), DataFrame, delim = ';', header = true)
end

# ╔═╡ 77b954d0-0f35-11eb-29ee-c510bd4e19e0
md"Here are our variable names:"

# ╔═╡ 88fa23f0-0f35-11eb-1c44-6561b6b01d19
names(data)

# ╔═╡ 6d2b0330-0e75-11eb-340d-d98ee8999fbd
md"Good, we have our data loaded, and it looks like it has been tidied, so let’s explore! Let’s begin with looking at the distribution of scores. We can do this a couple of ways, but lets begin with a histogram:"

# ╔═╡ 7b091f50-0e75-11eb-3844-f70945af9b92
histogram(data.score, bins = 5:1:19, color = :purple)

# ╔═╡ c1d08490-0e76-11eb-38f1-1d1c632f47ef
md"We can see that the distribution of wine scores is peaked around 15, some scores are around or above 18, with lowest scores around 6. Looking at this histogram, my gut feeling says the average wine score is around 14...lets check my intution:"

# ╔═╡ 61e83920-0e7a-11eb-1457-6dfcbe97a33c
mean(data.score)

# ╔═╡ 6721f610-0e7a-11eb-1f11-7b7eb3de1b1a
md"Nice, my years of studying the sith holocrons are paying off..." 

# ╔═╡ 9c2e2180-0e7a-11eb-1615-6f537377f1e5
md"Lets continue looking at the distribution of wine scores, but maybe visualize it differently. Lets draw a box plot for each wine flight"

# ╔═╡ 2dd6f300-0e7b-11eb-23d9-83fd93fd2d2a
boxplot(data.flight,data.score, color = [:purple :gold], group = data.flight, leg = false )

# ╔═╡ 8f16bc40-0f34-11eb-1bc3-19bb0ca2d53f
md"Looks like both red and white wines have similar scores, but the range for reds is much wider...reds have both the highest and lowest scores. Let’s double check this and see if there is a preference for American or French wines"

# ╔═╡ af7fbd50-0e7c-11eb-2a5e-cfdb3bc6d16c
begin
	grpdf = groupby(data, [:flight, Symbol("wine.amer")])
	grpdf = combine(grpdf, :score => mean => :avg_score, :score => std => :std_dev)
end

# ╔═╡ 74996bb0-0e7b-11eb-1566-3181befb8d3b
md"Interesting, looks like American reds have a lower average score and the largest standard deviation. We are starting to see that American reds are noticibily different. Another variable of interest is the origin of the judge. Maybe French judges can taste the difference in American vs French wines... "

# ╔═╡ 946d7610-0f3f-11eb-307b-77bf8ce917f8
begin
	gdf = groupby(data, [:flight, Symbol("wine.amer"), Symbol("judge.amer")])
	gdf = combine(gdf, :score => mean => :avg_score)
	unstack(gdf, [:flight, Symbol("wine.amer")], Symbol("judge.amer"), :avg_score,)
end

# ╔═╡ 245c3b80-0f40-11eb-1463-2b84bdd5e2b8
md"Ok...looks like European judges can really taste the difference between American and French reds. This distinction extends to Americans apparently too, as they also seemingly prefer French red wines."

# ╔═╡ fd51c270-1c74-11eb-3300-c97e5a635747
md"Lets see which wines won this tasting..."

# ╔═╡ 01800700-1c6e-11eb-3296-67870c1ef022
begin
	grp = groupby(data, [:wine, :flight])
	w = combine(grp, :score => sum => :t_score)
	r = ordinalrank(w.t_score[w.flight .== "white"], rev = true)
	append!(r,ordinalrank(w.t_score[w.flight .== "red"], rev = true))
	w.rankings = r

	w = sort(filter(x -> x.rankings .<= 3, w), :rankings)
	w.wine[1:end,:] = ["Clos des Mouches 2009", "Ch. Mouton Rothschild 2004",
		"Unionville Pheasant Hill Single Vineyard 2010","Ch. Haut Brion 2004","Puligny Montrachet Domaine Leflaive 2009", "Heritage Estate  BDX 2010"]
	w
end

# ╔═╡ ae595fa0-1c7b-11eb-3007-85ba47b04aef
md"Now that we have performed some basic EDA, lets try to model wine score. Because the response variable is continuous, we can use a linear regression to do this. The primary aim of this regression is to unmask any potential bias amongst judges. We have defined 3 interaction models:"

# ╔═╡ 94d790f0-109c-11eb-0bd1-8557da0535b1
md"Model 1:

	W_score ~ Normal(mu_i,sigma)
		mu_i = alpha[judge] + beta_wine[wine]
			 alpha[judge] ~ Normal(14,5)
			 beta[wine] ~ Normal(0,2.5)
			 sigma ~ Exponential(1)"

# ╔═╡ 2f061610-109d-11eb-0d82-ad1f9d4e76a7
md"Model 2:
	
	W_score ~ Normal(mu_i,sigma)
		mu_i = alpha[flight] + beta_wineAmerican[wine.american] + 
			beta_judgeAmerican[judge.ameri]
			alpha[flight] ~ Normal(14,5)
			beta_wineAmerican[wine.ameri] ~ Normal(0,2.5)
			beta_judgeAmerican[judge.ameri] ~ Normal(0,2.5)
			sigma ~ Exponential(1)
"

# ╔═╡ b4c40ff0-109d-11eb-270f-6b509d65f1db
md"   
Model 3: 

	W_score ~ Normal(mu_i,sigma)
	 	 mu_i = alpha + beta_wineAmerican*wineAmerican + beta_judgeAmerican*judgeAmerican + beta_flight*flight + beta_wAjA*wineAmerican*judgeAmerican + beta_wAfA*wineAmerican*flight + beta_fjA*flight*judgeAmerican

			 alpha ~ Normal(14,5)
			 beta_wineAmerican ~ Normal(0,2.5)
			 beta_judgeAmerican ~ Normal(0,2.5)
			 beta_flight ~ Normal(0,2.5)
			 beta_wAjA ~ Normal(0,2.5)
			 beta_wAf ~ Normal(0,2.5)
			 beta_fjA ~ Normal(0,2.5)
			 sigma ~ Exponential(1)
"

# ╔═╡ 1c8f90a0-109e-11eb-3c8b-3dc624f45cb9
md"Ok, now that we have our interaction models defined, lets simulate our priors to see if we're in the relm of possible wine scores... "

# ╔═╡ 5421fc50-109f-11eb-0d52-d323a49dc8a3
begin
	prior_alpha = Normal(14,5)
	prior_beta = Normal(0,2.5)
	
	scatter(data.judge,[rand(prior_alpha) .+ rand(prior_beta) for i in 1:50],
		leg = false, title = "Prior Predictive Simulation", color = :purple)
end


# ╔═╡ 3fc49900-10a1-11eb-189e-c1f3854f116e
md"Looks like our priors fit within the realm of possibilities, lets; run all three models and see if we can infer any bias.

Before we run our regression, we need to construct an index variable for wine & judge. As well as construct two new re-ordered variables for wine.amer & judge.amer."

# ╔═╡ d74e3ce0-10a1-11eb-0b39-21b67f87a618
begin
	data.w_indx = indexin(data.wine, unique(data.wine))
	data.j_indx = indexin(data.judge, unique(data.judge))
	data.w_amer = data."wine.amer" .+ 1
	data.j_amer = data."judge.amer" .+ 1
	data.f_index = indexin(data.flight,unique(data.flight))
	data.f = ifelse.(data.flight .== "red",1,0)
end

# ╔═╡ 3bbb8520-10a7-11eb-1d79-aba334167011
md"Lets model! Here is our first interaction model, "

# ╔═╡ 520982f0-10a7-11eb-1dd5-e993344c5f45
begin
	@model lm_1(judge,wine,score) = begin
		j = length(unique(judge))
		w = length(unique(wine))
		
		#priors
		a_judge ~ filldist(Normal(14,5),j)
		b_wine ~ filldist(Normal(0,2.5),w)
		sigma ~ Exponential(1)
		
		#model
			mu = a_judge[judge] .+ b_wine[wine]
			score ~ MvNormal(mu,sigma)
		
	end
end

# ╔═╡ 2500663e-10aa-11eb-3b62-dbe80b50d46a
begin
	chn1 = sample(lm_1(data.j_indx,data.w_indx,data.score),NUTS(.65),5000)
	alpha1 = Array(group(chn1, :a_judge))
	beta1 = Array(group(chn1, :b_wine))
	sigma1 = Array(group(chn1, :sigma))
	
	chn1
end
	

# ╔═╡ 5dc47c20-1a17-11eb-2d51-49e2a23112f6
md"Here are several density plots for judges and their expected wine scores. Our first model predicts John Foy to have more favorible scores then all judges...We'll explore this below."

# ╔═╡ 5d4be9a0-1228-11eb-243a-058f67a39b9b
begin
	p = Array{Plots.Plot{Plots.GRBackend}}(undef,9)
	judges = unique(data.judge)
	for i in 1:length(p)
	p[i] = density(alpha1[:,i], label = judges[i], color = :purple, 
			fill = (0.2,0,:midnightblue))
	end
	plot(p...,layout = (3,3))
end

# ╔═╡ e9ecc9c2-1df3-11eb-03a4-39982ce2212b
md"There are four judges with noticeably different expected scores than Foy. 50% judges are American, I don't believe this difference stems from growing up drinking French wines but rather Foy having a very different palate comparatively.

Here we compute posterior constrasts of the 4 judges...Cardebat & Hodgson both have less excited palates than Foy"


# ╔═╡ 2c23a610-1d13-11eb-2fd6-5d2e13b7f19d
begin
	dp = Array{Any,1}(undef,9)
	for i in 1:size(alpha1,2)
	dp[i] = density(alpha1[:,i] .- alpha1[:,3], color = :purple, 
			fill = (0.2,0,:midnightblue), label = "$(judges[i]) vs Foy")
	end
	plot(dp[[1,2,5,7],:]...)
end

# ╔═╡ 65d0d200-1df1-11eb-0cb3-a32f967e53e5
md"Let’s check our posterior predictions by visualizing what our model expects for judge scores. The purple shaded regions are the expected average scores (predictions for parameter mu), the open white circles are our model predicts what judges will score, and the gold stars are actual wine scores." 

# ╔═╡ 98cd8ae0-126b-11eb-1b42-31596cc97e7b
begin
	mu = [alpha1[i,data.j_indx] + beta1[i,data.w_indx] for i in 1:1000]
	s_predict = rand.(Normal.(mu[1],sigma1[1:180]))
	violin(data.judge,mu[1],color = :midnightblue, alpha = .25, leg = false)
	violin!(data.judge,s_predict, color = :gold2, alpha = .20)
	scatter!(data.judge, data.score, color = :gold, marker = :star)
	
end
	

# ╔═╡ af0e71f0-1df3-11eb-275e-8b24d2d774da
md"Ok...onto interaction model two! Here we're modelling wine score again, but this time we are using flight and two indicator variables; Judge American(1 = French | 2 = American ) and Wine American. We're looking to see how American and French wines and judges affect score. Here is the model and summary statistics." 

# ╔═╡ cc6f2230-130d-11eb-1f89-f7af65998731
begin
	@model lm_2(flight,judge_ameri,wine_ameri,score) = begin
		f, ja, wa = length(unique(flight)), 
		length(unique(judge_ameri)), length(unique(wine_ameri))
		
		#priors
		a_flight ~ filldist(Normal(14,5),f)
		beta_jA ~ filldist(Normal(0,2.5),ja)
		beta_wA ~ filldist(Normal(0,2.5),wa)
		sigma ~ Exponential(1)
		
		#model
		mu = a_flight[flight] .+ beta_jA[judge_ameri] .+ beta_wA[wine_ameri]
		score .~ Normal.(mu,sigma)
		end
	
	model_2 = lm_2(data.f_index,data.j_amer,data.w_amer,data.score)
	chn2 = sample(model_2,NUTS(.65),5000)
	
		alpha_2 = Array(group(chn2,:a_flight))
		beta_jA = Array(group(chn2,:beta_jA))
		beta_wA = Array(group(chn2,:beta_wA))
		sigma2 = Array(group(chn2,:sigma))
		chn2
		
end
	

# ╔═╡ 09a5bca2-1655-11eb-04fe-a1841dd78f8f
md"Lets plot the posterior distributions for French & American judges along with a constrast between them. Looks like we have statstical evidence to support the claim that indeed, French judges are more biased than their American counterparts. The constrast is reliabibly below zero, with the posterior distribution narrowly centered around -0.65. "

# ╔═╡ c3d765e0-14bd-11eb-0bb3-d3c0cdb4f5e2
begin
	density(beta_jA[:,1], labels = "French Judge", color = :purple, linestyle = :dot)
	density!(beta_jA[:,2], labels = "American Judge", color = :purple)
	density!(beta_jA[:,1] .- beta_jA[:,2], color = :purple, 
		labels = "Contrast: French vs American Judges")
end

# ╔═╡ 9a59f280-1e38-11eb-32ce-c75a4b967a60
md"Let’s plot the posterior distributions for French & American judges along with the contrast between them. Looks like we have statistical evidence to support the claim that indeed, French judges are more biased than their American counterparts. The contrasts is reliably below zero, with the posterior distribution narrowly centered around -0.65."

# ╔═╡ 4257a6d0-22f3-11eb-106c-d90f3ed1253b
md"Let's plot model 2 predictions and expected scores. Here are 1,000 predictions for Red and White wines."

# ╔═╡ bc4ea45e-1403-11eb-033d-a9c43bd68375
begin
	mu2 = hcat([alpha_2[i,data.f_index] .+ beta_jA[i,data.j_amer] .+ 
		beta_wA[i,data.w_amer] for i in 1:1000]...)
		
	y_score2 = rand.(Normal.(mu2,mean(sigma2)))
	
	violin(data.flight,y_score2, color = :purple, alpha = .10, leg = false)
	violin!(data.flight,mu2, alpha = .05, color = :gold2)
	
end

# ╔═╡ d967f4d0-1e39-11eb-3e6e-c3af921f5008
md"Finally, our last interaction model we throw everything from the previous models into one long, exhaustive interaction model. We consider three-two-way interactions; how American judges interact with flight, how American wines interact with flight, and how American wines and American judges interact."

# ╔═╡ b31b5630-1404-11eb-2f9c-13800030fcab
begin
	@model lm_3(flight,judge_ameri,wine_ameri,score) = begin
		alpha ~ Normal(14,5)
		beta_wA ~ Normal(0,2.5)
		beta_jA ~ Normal(0,2.5)
		beta_flight ~ Normal(0,2.5)
		beta_wAjA ~ Normal(0,2.5)
		beta_wAflight ~ Normal(0,2.5)
		beta_flightjA ~ Normal(0,2.5)
		sigma ~ Exponential(1)
		
		mu = alpha .+ beta_wA .* wine_ameri .+ beta_jA .* judge_ameri 
			.+ beta_flight .* flight .+ beta_wAjA .* wine_ameri.*judge_ameri
			.+ beta_wAflight .* wine_ameri .* flight .+ beta_flightjA .* 
			flight .* judge_ameri
		
		score .~ Normal.(mu,sigma)
	end
	
	model_3 = lm_3(data.f,data."judge.amer",data."wine.amer",data.score)
	chn3 = sample(model_3,NUTS(),5000)
	
end

# ╔═╡ 0bd657a0-22df-11eb-26ec-431313af4dd8
md"Let's look at the expected wine scores for the various categories in the dataset. Consider the following arrangement of indicator variables:"

# ╔═╡ e8c20460-22e0-11eb-0a24-071f0d18a714
md"
**ffw** = French Wine | French Judge | White Wine

**afw** - American Wine | French Judge | White Wine

**ffr** - French Wine | French Judge | Red Wine

**afr** - American Wine | French Judge | Red Wine

**faw** - French Wine | American Judge | white Wine

**aaw** - American Wine | American Judge | White Wine

**far** - French Wine | American Judge | Red Wine

**aar** - American Wine | American Judge | Red Wine
"

# ╔═╡ c5078d90-1ee6-11eb-397c-adaa0cc804dc
begin
	
	alpha3 = Array(group(chn3, :alpha))
	bwA = Array(group(chn3, :beta_wA))
	bJa = Array(group(chn3, :beta_jA))
	bf = Array(group(chn3, :beta_flight))
	bwAJa = Array(group(chn3, :beta_wAjA))
	bfJa = Array(group(chn3, :beta_flightjA))
	bwAf = Array(group(chn3, :beta_wAflight))
	sigma3 = Array(group(chn3, :sigma))
	
	fake_df = DataFrame(w = repeat([0,1], outer = 4), j = repeat([0,1], inner = 4),
		r = repeat([0,1], inner =2, outer = 2))
	
	mu3 = hcat([alpha3[i] .+ bwA[i] .* fake_df.w .+ bJa[i] .* fake_df.j .+ bf[i] .* fake_df.r .+ bwAJa[i] .* fake_df.w .* fake_df.j .+ bfJa[i] .* fake_df.r .* fake_df.j .+ bwAf[i] .* fake_df.w .* fake_df.r for i in 1:1000]...)
	
	n = string.(ifelse.(fake_df.w .== 1, "a", "f"), ifelse.(fake_df.j .== 1, "a", "f"),ifelse.(fake_df.r .== 1, "r", "w"))
	
	fake_df.cat = n
	
	violin(n,mean(mu3, dims = 1)', color = :purple, leg = false)

	end


# ╔═╡ 93c40d50-22e0-11eb-1cb4-31548ec77a0d
fake_df

# ╔═╡ f9c41670-22d8-11eb-26ad-c9863d4f4dac
md"The Violin plots above show that model 3 expects French Judges to really enjoy French Red Wines. This may in fact be proof that the French Judges in the dataset have a preference for wine's they are accustomed too. American Judges don't seem to enjoy American White Wines too much, but their French counterparts do!" 

# ╔═╡ Cell order:
# ╟─ffd2bd20-0cbf-11eb-3f27-61c959ab7310
# ╟─da0002a0-0cc0-11eb-2ad6-8d91c6735423
# ╟─b1f22000-0cd3-11eb-3439-470d4fcb1f4f
# ╟─f0bfd320-1ab8-11eb-3166-ef0113e3289b
# ╟─798cb770-0cd3-11eb-1e8f-a7710269a913
# ╠═fc493a80-0cba-11eb-257b-bdd6b4439dd1
# ╟─93e9e8d0-0cbb-11eb-09b9-3bb9799867b8
# ╠═b72c6c80-0e70-11eb-39f0-87f276855ce6
# ╟─77b954d0-0f35-11eb-29ee-c510bd4e19e0
# ╟─88fa23f0-0f35-11eb-1c44-6561b6b01d19
# ╟─6d2b0330-0e75-11eb-340d-d98ee8999fbd
# ╠═7b091f50-0e75-11eb-3844-f70945af9b92
# ╠═c1d08490-0e76-11eb-38f1-1d1c632f47ef
# ╠═61e83920-0e7a-11eb-1457-6dfcbe97a33c
# ╟─6721f610-0e7a-11eb-1f11-7b7eb3de1b1a
# ╟─9c2e2180-0e7a-11eb-1615-6f537377f1e5
# ╠═2dd6f300-0e7b-11eb-23d9-83fd93fd2d2a
# ╟─8f16bc40-0f34-11eb-1bc3-19bb0ca2d53f
# ╠═af7fbd50-0e7c-11eb-2a5e-cfdb3bc6d16c
# ╟─74996bb0-0e7b-11eb-1566-3181befb8d3b
# ╠═946d7610-0f3f-11eb-307b-77bf8ce917f8
# ╟─245c3b80-0f40-11eb-1463-2b84bdd5e2b8
# ╟─fd51c270-1c74-11eb-3300-c97e5a635747
# ╠═01800700-1c6e-11eb-3296-67870c1ef022
# ╟─ae595fa0-1c7b-11eb-3007-85ba47b04aef
# ╟─94d790f0-109c-11eb-0bd1-8557da0535b1
# ╟─2f061610-109d-11eb-0d82-ad1f9d4e76a7
# ╟─b4c40ff0-109d-11eb-270f-6b509d65f1db
# ╟─1c8f90a0-109e-11eb-3c8b-3dc624f45cb9
# ╠═5421fc50-109f-11eb-0d52-d323a49dc8a3
# ╟─3fc49900-10a1-11eb-189e-c1f3854f116e
# ╠═d74e3ce0-10a1-11eb-0b39-21b67f87a618
# ╟─3bbb8520-10a7-11eb-1d79-aba334167011
# ╠═520982f0-10a7-11eb-1dd5-e993344c5f45
# ╠═2500663e-10aa-11eb-3b62-dbe80b50d46a
# ╟─5dc47c20-1a17-11eb-2d51-49e2a23112f6
# ╠═5d4be9a0-1228-11eb-243a-058f67a39b9b
# ╟─e9ecc9c2-1df3-11eb-03a4-39982ce2212b
# ╠═2c23a610-1d13-11eb-2fd6-5d2e13b7f19d
# ╟─65d0d200-1df1-11eb-0cb3-a32f967e53e5
# ╠═98cd8ae0-126b-11eb-1b42-31596cc97e7b
# ╟─af0e71f0-1df3-11eb-275e-8b24d2d774da
# ╠═cc6f2230-130d-11eb-1f89-f7af65998731
# ╟─09a5bca2-1655-11eb-04fe-a1841dd78f8f
# ╠═c3d765e0-14bd-11eb-0bb3-d3c0cdb4f5e2
# ╟─9a59f280-1e38-11eb-32ce-c75a4b967a60
# ╟─4257a6d0-22f3-11eb-106c-d90f3ed1253b
# ╠═bc4ea45e-1403-11eb-033d-a9c43bd68375
# ╟─d967f4d0-1e39-11eb-3e6e-c3af921f5008
# ╠═b31b5630-1404-11eb-2f9c-13800030fcab
# ╟─0bd657a0-22df-11eb-26ec-431313af4dd8
# ╟─93c40d50-22e0-11eb-1cb4-31548ec77a0d
# ╟─e8c20460-22e0-11eb-0a24-071f0d18a714
# ╠═c5078d90-1ee6-11eb-397c-adaa0cc804dc
# ╟─f9c41670-22d8-11eb-26ad-c9863d4f4dac
