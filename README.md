# 🏗️ Real Estate Crowdfunding Pool (RECP) Smart Contract

## 🎯 Overview
RECP is a decentralized crowdfunding platform for real estate development projects built on Stacks blockchain. It enables transparent fundraising and investment management for property development initiatives.

## ✨ Features
- 🏢 Create real estate development projects
- 💰 Accept STX investments
- 🔄 Automatic fund management
- 🔒 Secure refund mechanism
- 📊 Investment tracking

## 🚀 Usage

### For Project Owners
1. Create a new project:
```clarity
(contract-call? .recp create-project "Luxury Apartments" u1000000000)
```

### For Investors
1. Invest in a project:
```clarity
(contract-call? .recp invest u1)
```

2. Claim refund (if project fails):
```clarity
(contract-call? .recp claim-refund u1)
```

## 🔑 Key Parameters
- Minimum investment: 1,000,000 microSTX
- Maximum projects: 100
- Funding period: 144 blocks
- Success threshold: 800,000,000 microSTX

## 🛠️ Technical Details
- Contract owner can create and finalize projects
- Automatic refund mechanism for failed projects
- Real-time investment tracking
- Secure fund management

## 🔐 Security
- Owner-only administrative functions
- Protected investment operations
- Automatic status management

