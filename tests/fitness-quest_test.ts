import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.4.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.170.0/testing/asserts.ts';

Clarinet.test({
  name: "Facet-Fetch: Fitness Quest Core Test Suite",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const admin = accounts.get('wallet_1')!;
    const participant1 = accounts.get('wallet_2')!;
    const participant2 = accounts.get('wallet_3')!;

    // Basic quest creation test
    let block = chain.mineBlock([
      Tx.contractCall('fitness-quest', 'grant-role', 
        [types.ascii('platform-admin'), types.principal(admin.address)], 
        admin.address
      )
    ]);
    
    // Simulate quest enrollment
    block = chain.mineBlock([
      Tx.contractCall('fitness-quest', 'enroll-in-quest', 
        [types.uint(1)], 
        participant1.address
      )
    ]);

    // Validate basic quest mechanics
    assertEquals(block.receipts[0].result, '(ok true)');
  }
});